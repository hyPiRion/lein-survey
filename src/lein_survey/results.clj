(ns lein-survey.results
  (:require [clojure.set] ; work around incanter bug
            [clojure.java.jdbc :as sql]
            [clojure.pprint :as pprint]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [incanter.core :as incanter]
            [incanter.charts :as charts]
            [lein-survey.questions :as q])
  (:import (org.apache.commons.codec.digest DigestUtils)))

(def cache (atom {:rev 0}))

(defn flush-cache! []
  (swap! cache (fn [c] {:rev ((fnil inc 0) (:rev c))})))

(defn memoize-in
  "Like memoize, but inside a specified atom instead. The atom can be
  used for memoization of multiple functions."
  [mem f]
  (fn [& args]
    (let [rev (:rev @mem)]
      (if-let [e (find @mem [f rev args])]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc [f rev args] ret)
          ret)))))

(defn setize [x]
  (if (coll? x)
    (set x)
    (hash-set x)))

(defn merge-results [{:keys [id body timestamp]}]
  (assoc (read-string body) :id id :timestamp (.getTime timestamp)))

(def results-str
  (memoize-in
   cache
   (fn []
     (sql/with-connection (or (System/getenv "DATABASE_URL")
                              "postgres://localhost:5432/lein-survey")
       (sql/with-query-results results ["select * from answers"]
         (pr-str (map merge-results results)))))))

(def results-url (java.net.URL. "http://lein-survey-2014.herokuapp.com/results.clj"))

(defonce get-results
  (memoize-in cache
   (fn [] (read-string (results-str)))))

(defn hash-question [q]
  (subs (DigestUtils/shaHex q) 10))

(defn commentary [q]
  #_(if-let [c (io/resource (str "commentary/2015/" (hash-question q)))]
    [:p (slurp c)]))

(defn img-link [q]
  (format "/%s.png" (hash-question q)))

(defn percent-freqs [f choice results]
  (let [n (or (f choice) 0)
        total (count results)]
    (if (zero? total)
      (format "0")
      (format "%s (%s%%)" n (int (* 100 (/ n total)))))))

(defmulti summarize-question (fn [results question] (second question)))

(defmethod summarize-question :radio [results [q _ choices]]
  (let [freqs (frequencies (for [r results] (get r q)))]
    [:div.answer
     [:img {:src (img-link q) :align "right"}]
     [:h4.question q]
     [:dl (apply concat (for [choice choices]
                          [[:dt choice] [:dd (percent-freqs
                                              freqs choice results)]]))]
     (commentary q)]))

(defmethod summarize-question :check [results [q _ choices]]
  (let [results-sets (apply concat (for [r results] (setize (get r q))))
        freqs (frequencies results-sets)]
    [:div.answer
     [:img {:src (img-link q) :align "right"}]
     [:h4.question q]
     [:dl (apply concat (for [choice choices]
                          [[:dt choice] [:dd (percent-freqs
                                              freqs choice results)]]))]
     (commentary q)]))

;;; summarizing plugins
;; (->> (mapcat #(.split % "[, ]+") p)
;;      (map #(string/replace % "lein-" ""))
;;      (map (memfn toLowerCase))
;;      (frequencies)
;;      (filter (comp pos? dec second))
;;      (sort-by second)
;;      (reverse)
;;      (rest)
;;      (pprint))

(defmethod summarize-question :textarea [results [q _ choices]]
  (if-let [s (io/resource (case q
                            "Other comments?"
                            "commentary/2015/other.html"
                            "Favourite plugins? (comma-separated)"
                            "commentary/2015/plugins.html"
                            "Favourite templates? (comma-separated)"
                            "commentary/2015/templates.html"))]
    [:div.answer (slurp s)]))

(defmethod summarize-question :rank [results [q _ choices]]
  (let [freqs #(sort-by (comp first key)
                        (frequencies (for [r results]
                                       (setize (get r (str q " " %))))))]
    [:div.answer
     [:h4.question q]
     [:ul (for [choice choices]
            [:li choice
             (for [[rank count] (freqs choice)
                   :when (not= rank #{nil})]
               (str " | " count))])]
     [:p "The chart's missing for this one on account of getting in a fight "
      "with Incanter's stacked bar chart and losing."]
     (commentary q)]))

(defn summary []
  (let [results (get-results)]
    (into [:div.summary
           [:h3 "Data and commentary on the results"]
           [:p ;; "The survey ran from the 24th of March to the end of April. "
            ;; "A final summary of commentary will be posted soon."
            " The survey is still open; commentary will be posted once it closes."
            " Most questions allowed more than one answer, so percentages"
            " will not add up to 100."]
           [:p [:a {:href "http://lein-survey-2014.herokuapp.com"}
                "Last year's survey results are still up."]]
           [:p "It may be interesting to compare some of these results "
            "with Cognitect's "
            [:a {:href "http://blog.cognitect.com/blog/2014/10/20/results-of-2014-state-of-clojure-and-clojurescript-survey"}
             "State of Clojure"] " survey from last November."]
           [:p "You can see "
            [:a {:href "https://github.com/technomancy/lein-survey"}
             "the source"] " for this survey on Github or get the "
            [:a {:href "/results.clj"} "raw results"]
            " for your own analysis."]
           [:p "Total responses: " (count results)]]
          (map (partial summarize-question results) q/questions))))

(def os-map {"Debian/Ubuntu" :linux
             "Fedora/other RPM-based" :linux
             "Arch" :linux
             "Gentoo" :linux
             ;; "Nix" :other
             "Other GNU/Linux" :linux
             "Mac OS X with Homebrew" :mac
             "Mac OS X with Macports" :mac
             "Mac OS X with Fink" :mac
             "Mac OS X with no package manager" :mac
             "Windows with Powershell" :windows
             "Windows without Powershell" :windows
             "Windows with Cygwin" :windows
             ;; "Solaris" :other
             ;; "BSD" :other
             ;; "other" :other
             })

(defn os-lookup [q result]
  (let [q-result (get result q)
        result-set (if (coll? q-result)
                     (set q-result)
                     (hash-set q-result))]
    (set (for [[name type] os-map
               :when (result-set name)]
           type))))

(defn pie
  ([q results] (pie q results (fn [q result] (get result q))))
  ([q results lookup]
     (let [freqs (dissoc (frequencies (map (partial lookup q) results))
                         nil "I don't remember" #{})
           freqs (sort-by (comp str key) freqs)
           labels (for [l (keys freqs)]
                    (first (.split (str l) "\\(")))]
       (charts/pie-chart labels (vals freqs)
                         :title q :legend true))))

(defn bar
  ([q results] (bar q results (fn [q result] (get result q))))
  ([q results lookup]
     (let [q-results (map (partial lookup q) results)
           q-results-spread (apply concat (for [r q-results]
                                            (if (coll? r) r [r])))
           freqs (dissoc (frequencies q-results-spread)
                         nil "I don't remember" #{} [] "")
           freqs (sort-by (comp str key) freqs)
           ;; TODO: make threshhold customizable?
           freqs (filter (fn [[x n]] (> n 3)) freqs)]
       (if (seq freqs)
         (charts/bar-chart (map str (keys freqs)) (vals freqs)
                           :x-label "" :y-label ""
                           :title q :vertical false)))))

(defn get-ranks [result]
  (into {} (for [[q rank] result
                 :when (.startsWith (name q) "Rank your biggest")
                 :let [annoyance (second (.split (name q) ": "))]]
             [annoyance rank])))

(defn stacked-bar [q results]
  (let [ranks (map get-ranks results)
        categories (keys (first ranks))
        values (for [category categories
                     ranking ranks
                     [annoyance number] ranking
                     :when (= category annoyance)]
                 [category number])
        ;; rankings (reduce (fn [acc [q n]]
        ;;                    (update-in acc [q] conj (Integer. n)))
        ;;                  (zipmap categories (repeat (count categories) []))
        ;;                  values)
        categories (map first values)
        values (map (comp #(Integer. %) second) values)]
    ;; grraaaah; stupid stacked bar charts can suck it
    (charts/stacked-bar-chart categories values
                              :x-label "" :y-label ""
                              :title "Biggest annoyances" :vertical false)))

(defn chart [q type results]
  (cond (= "How long have you been using Clojure?" q)
        (bar q results)
        (= :radio type)
        (pie q results)
        (= :check type)
        (bar q results)
        (= :os type)
        (pie q results os-lookup)
        ;; (= :rank type)
        ;; (stacked-bar q results)
        ))

(def hashed-questions (into {} (for [q q/questions]
                                 [(hash-question (first q)) q])))

(def image-bytes
  (memoize-in cache
   (fn [id]
     (let [[q type] (hashed-questions
                     id ["Your OS and package manager(s)" :os])
           out (java.io.ByteArrayOutputStream.)
           results (get-results)
           chart (chart q type results)]
       (if chart
         (incanter/save chart out)
         (io/copy (clojure.java.io/input-stream (io/resource "not-enough-data.png")) out))
       (.toByteArray out)))))

(defn image [id]
  (java.io.ByteArrayInputStream. (image-bytes id)))

(def comments
  (memoize-in
   cache
   (fn [] (string/join "\n----------------\n"
                      (for [body (get-results)]
                        (get body "Other comments?"))))))
