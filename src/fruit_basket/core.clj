(ns fruit-basket.core
  (:gen-class)
  (:require [clojure.math.numeric-tower :refer [expt]]
            [kixi.stats.distribution :refer [gamma sample]]))

(def ^:const default-fruit-types
  "Map of fruit types"
  {:green-apple {:key :green-apple :string "🍏" :points (* 5 0)     :level 0}
   :red-apple   {:key :red-apple   :string "🍎" :points (* 5 1)     :level 1}
   :pear        {:key :pear        :string "🍐" :points (* 5 2)     :level 2}
   :orange      {:key :orange      :string "🍊" :points (* 5 3)     :level 3}
   :lemon       {:key :lemon       :string "🍋" :points (* 5 4)     :level 4}
   :banana      {:key :banana      :string "🍌" :points (* 5 5)     :level 5}
   :watermelon  {:key :watermelon  :string "🍉" :points (* 5 6)     :level 6}
   :grapes      {:key :grapes      :string "🍇" :points (* 5 7)     :level 7}
   :strawberry  {:key :strawberry  :string "🍓" :points (* 5 8)     :level 8}
   :cantelope   {:key :cantelope   :string "🍈" :points (* 5 9)     :level 9}
   :cherry      {:key :cherry      :string "🍒" :points (* 5 10)    :level 10}
   :peach       {:key :peach       :string "🍑" :points (* 5 11)    :level 11}
   :mango       {:key :mango       :string "🥭" :points (* 5 12)    :level 12}
   :pineapple   {:key :pineapple   :string "🍍" :points (* 5 13)    :level 13}
   :coconut     {:key :coconut     :string "🥥" :points (* 5 14)    :level 14}
   :kiwi        {:key :kiwi        :string "🥝" :points (* 5 15)    :level 15}
   :tomato      {:key :tomato      :string "🍅" :points (* 5 16)    :level 16}
   :eggplant    {:key :eggplant    :string "🍆" :points (* 5 17)    :level 17}
   :avocado     {:key :avocado     :string "🥑" :points (* 5 18)    :level 18}
   :broccoli    {:key :broccoli    :string "🥦" :points (* 5 19)    :level 19}
   :lettuce     {:key :lettuce     :string "🥬" :points (* 5 20)    :level 20}
   :cucumber    {:key :cucumber    :string "🥒" :points (expt 11 2) :level 21}
   :pepper      {:key :pepper      :string "🌶" :points (expt 12 2) :level 22}
   :corn        {:key :corn        :string "🌽" :points (expt 13 2) :level 23}
   :carrot      {:key :carrot      :string "🥕" :points (expt 15 2) :level 25}
   :garlic      {:key :garlic      :string "🧄" :points (expt 16 2) :level 26}
   :onion       {:key :onion       :string "🧅" :points (expt 17 2) :level 27}
   :potato      {:key :potato      :string "🥔" :points (expt 18 2) :level 28}
   :yam         {:key :yam         :string "🍠" :points (expt 19 2) :level 29}
   :croissant   {:key :croissant   :string "🥐" :points (expt 20 2) :level 30}
   :bagel       {:key :bagel       :string "🥯" :points (expt 21 2) :level 31}
   :bread       {:key :bread       :string "🍞" :points (expt 8 3)  :level 32}
   :baguette    {:key :baguette    :string "🥖" :points (expt 9 3)  :level 33}
   :pretzel     {:key :pretzel     :string "🥨" :points (expt 10 3) :level 34}
   :egg         {:key :egg         :string "🥚" :points (expt 11 3) :level 35}
   :butter      {:key :butter      :string "🧈" :points (expt 12 3) :level 36}
   :pancakes    {:key :pancakes    :string "🥞" :points (expt 13 3) :level 37}
   :waffles     {:key :waffles     :string "🧇" :points (expt 14 3) :level 38}
   :fries       {:key :fries       :string "🍟" :points (expt 15 3) :level 39}
   :pizza       {:key :pizza       :string "🍕" :points (expt 16 3) :level 40}
   :sandwich    {:key :sandwich    :string "🥪" :points (expt 17 3) :level 41}
   :falafel     {:key :falafel     :string "🧆" :points (expt 18 3) :level 42}
   :rice-ball   {:key :rice-ball   :string "🍙" :points (expt 19 3) :level 43}})

(defn- luck-distribution
  [luck]
  (gamma {:shape 1 :scale luck}))

(defn- select-fruit
  "Select a fruit from a fruit-types map given the number of points"
  [fruit-types points]
  (->> fruit-types
       vals
       (filter #(<= (:points %) (max 0 points)))
       (apply max-key :points)))

(defn draw-fruits
  "Draw n random fruits from the fruit-types map given a luck score"
  [& {:keys [n luck fruit-types]
      :or {fruit-types default-fruit-types}}]
  (letfn [(draw-fruit [luck-val]
            (-> (select-fruit fruit-types luck-val)
                (assoc :luck-val luck-val
                       :luck-score luck)))]
    (->> (luck-distribution luck)
         (sample n)
         (map draw-fruit))))

(def ^:const default-max-fruits 5)

(defn top-up-fruits
  "Given a user, generate events which add fruits to that user up to the max"
  [& {:keys [user fruit-types max-fruits]
      :or {fruit-types default-fruit-types
           max-fruits default-max-fruits}}]
  (let [n (->> user
               :fruits
               count
               (- max-fruits)
               (max 0))
        fruit->event (fn [fruit]
                       {:event-type :add-fruit
                        :user-id (:id user)
                        :source :found
                        :fruit fruit})]
    (->> (draw-fruits :n n
                      :luck (:luck user)
                      :fruit-types fruit-types)
         (map fruit->event))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
