;;;; Tuomas Toivonen
;;;; 27.1.2019
;;;; Clojuren perusteiden itseopiskelua

;;;; Clojuren perusidiomit
;;;; 0. Lisp-murre JVM:ssa, epapuhdas funktionaalinen paradigma. REPL-vetoinen kehitys
;;;; 1. Yleisesti ei muuttujia tai tilan kasitetta. Persistentit tietorakenteet
;;;; 2. Keskeisimmat tietorakenteet: vector, list, map, set. Paljon funktioita em. rakenteille
;;;; 3. Korkeamman asteen funktiot ja rekursio keskiossa
;;;; 4. Makrot ja metaohjelmointi. Homoikoninen syntaksi
;;;; 5. Parhaimmillaan rinnakkaisohjelmoinnissa
;;;; 6. Pilkku (,) vastaa valilyontia! Kayta oman maun mukaan... :)

;; sekvenssi-rajapinnan improvisoitu, naiivi toteutus
;; iterator-, map-, fitler- ja reduce-funktioiden toteutus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nimetty funktio maaritellaan defn-operaatiolla
;; []-vektorissa esitellaan funktion argumentit
;; merkkijonolla dokumentoidaan funktio tarvittaessa
;; funktio evaluoi lausekkeita jarjestyksessa ja palauttaa viimeisen evaluoituneen arvon
;; funktiota kutsutaan formilla (funktio arg1 arg2 arg3 ...), arg = Literaali tai toinen form jne.
;; {...} = map-literaali, :value = keyword, assoc = funktio, joka liittaa mappiin uuden avain-arvo -parin
(defn my-cons [value my-seq]
  "cons-funktio liittaa sekvenssin alkuun uuden alkion"
  (assoc {:value value} :next my-seq))

;; taman funktion kutsu evaluoituu argumentikseen
(defn my-first [my-seq]
  "first-funktio palauttaa sekvenssin ensimmaisen alkion"
  my-seq)

;; keyword-tyyppista avainta voi kayttaa mapin funktiona arvon hakemiseksi
(defn my-rest [my-seq]
  "rest-funktio palauttaa loput sekvenssista"
  (:next my-seq))

;; tietorakenteiden kasittely funktionaalisella kielella perustuu rekursioon
;; rekursio paattyy pohjaehdon eli "base case":n tayttyessa
;; first- ja rest-funktiot toimivat tyypillisesti symbioosissa rekursiivisessa funktiossa
;; cons-funktiota kaytetaan tyypillisesti sekvenssin rakentamiseen rekursiivisesti
;; &-operaattori kokoaa kaikki annetut argumentit yhteen sekvenssiin (variadic-funktio)
;; let-operaatio evaluoi ja nimeaa arvot muodostaen niille leksikaalisen nakyvyysalueen
;; if-operaatio evaluoi ja palauttaa ensimmaisen (then) tai jalkimmaisen (else) lausekkeen
;; apply-funktio kutsuu funktiota sekvenssin sisaltamilla argumenteilla
;; -not -paatteiset ehtorakenteet ovat kaytannollisia koodin siisteyden kannalta
;; predikaattifunktiot paattyvat kysymysmerkkiin
(defn build-my-seq [& args]
  "muodostaa argumenteista linkitetyn listarakenteen"
  (let [head (first args) tail (rest args)]
    (if-not (empty? tail)
      (my-cons head (apply build-my-seq tail))
      (my-cons head nil)))) ;; base case

;; when-operaatio suorittaa kaikki lausekkeet jarjestyksessa, eika sisalla else-haaraa
(defn my-iter [my-seq f]
  "iteroi sekvenssin kutsuen funktiota jokaiselle alkiolle"
  (when-not (nil? my-seq)
    (f (my-first my-seq))
    (my-iter (my-rest my-seq) f)))

;; rekursio ja co. taas vauhdissa!
(defn my-map [my-seq f]
  "mappaa sekvenssin funktiolla palauttaen uuden sekvenssin"
  (when-not (nil? my-seq)
    (my-cons (f (:value (my-first my-seq))) (my-map (my-rest my-seq) f))))

;; loop/recur -rakenteella voidaan toteuttaa iteraatio rekursiivisesti
;; loop toimii kuten let, ja recur-kasky siirtyy loopin alkuun uusilla argumenteilla
;; recur-kaskya voidaan kutsua vain hanta-positiosta. Clojure optimoi hanta-rekursion.
;; tassa let antaa anonyymille funktiomaaritykselle nimen
(defn my-filter [my-seq p?]
  "suodattaa sekvenssin predikaatilla palauttaen uuden sekvenssin"
  (let [find-next (fn [my-seq]
        (loop [remaining my-seq]
          (when-not (empty? remaining)
            (let [head (my-first remaining)]
              (if (p? (:value head))
                head
                (recur (my-rest head)))))))]
    (when-not (nil? my-seq)
      (when-let [next (find-next my-seq)]
        (my-cons (:value next) (my-filter (my-rest next) p?))))))

;; korkeamman tason funktio, joka palauttaa funktion
;; partial-funktiolla voidaan kiinnittaa funktion argumentti
(defn make-my-mul [n]
  "palauttaa funktion, joka kertoo argumenttinsa n:lla"
  (partial * n))

;; testataan custom-sekvenssin toiminta
(let [my-list-binding (apply build-my-seq (range 10))
      my-mul-2 (make-my-mul 2)]
  (my-iter my-list-binding #(println %1))
  (my-map my-list-binding my-mul-2)
  (my-filter my-list-binding even?))