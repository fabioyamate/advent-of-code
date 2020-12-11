(ns advent-of-code-2020.day04
  (:use clojure.test)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def blank #"\S+")

(re-seq blank "foo:foo bar:car zar:sdsd#\nca:sd\n\nfo:s")

(def sample "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(defn read-input
  [data]
  (->> (str/split data #"\n\n")
       (map #(re-seq #"\S+" %))
       (map (fn [col]
              (map #(str/split % #"\:") col)))
       (map #(into {} %))))

(defn valid-1?
  [passport]
  (or (= 8 (count passport))
      (and (= 7 (count passport))
           (not (contains? passport "cid")))))

(defn solve
  [f data]
  (count (filter identity (map f (read-input data)))))

(defn byr-rule
  [value]
  (<= 1920 (Integer/parseInt value) 2002))

(defn iyr-rule
  [value]
  (<= 2010 (Integer/parseInt value) 2020))

(defn eyr-rule
  [value]
  (<= 2020 (Integer/parseInt value) 2030))

(defn hgt-rule
  [value]
  (when-some [v (re-find #"[0-9]+" value)]
    (cond (str/ends-with? value "cm")
          (<= 150 (Integer/parseInt v) 193)

          (str/ends-with? value "in")
          (<= 59 (Integer/parseInt v) 76))))

(defn hcl-rule
  [value]
  (re-matches #"#[0-9a-f]{6}" value))

(def ecl-rule #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(defn pid-rule
  [value]
  (re-matches #"[0-9]{9}" value))

(defn validate-field
  [field validates-rule]
  (fn [passport]
    (when-some [value (get passport field)]
      (validates-rule value))))

(def valid-2?
  (every-pred (validate-field "byr" byr-rule)
              (validate-field "iyr" iyr-rule)
              (validate-field "eyr" eyr-rule)
              (validate-field "hgt" hgt-rule)
              (validate-field "hcl" hcl-rule)
              (validate-field "ecl" ecl-rule)
              (validate-field "pid" pid-rule)))

(deftest solve-1-test
  (is (= [true false true false]
         (map valid-1? (read-input sample))))

  (is (= 2 (solve valid-1? sample)))

  (is (= 196 (solve valid-1? (slurp (io/resource "advent-of-code-2020/day04.in"))))))

(def invalid-sample
  "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")

(def valid-sample
  "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")


(deftest rules-test
  (is (byr-rule "2002"))
  (is (not (byr-rule "2003")))

  (is (hgt-rule "60in"))
  (is (hgt-rule "190cm"))
  (is (not (hgt-rule "190in")))
  (is (not (hgt-rule "190")))

  (is (hcl-rule "#123abc"))
  (is (not (hcl-rule "#123abz")))
  (is (not (hcl-rule "123abc")))

  (is (ecl-rule "brn"))
  (is (not (ecl-rule "wat")))

  (is (pid-rule "000000001"))
  (is (not (pid-rule "0123456789")))
  )

(deftest solve-2-test
  (is (= 2 (solve valid-2? sample)))
  (is (= 0 (solve valid-2? invalid-sample)))
  (is (= 4 (solve valid-2? valid-sample)))

  (is (= 114 (solve valid-2? (slurp (io/resource "advent-of-code-2020/day04.in"))))))
