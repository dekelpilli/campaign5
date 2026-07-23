(ns campaign5.randoms
  (:require
    [methodical.core :as m]
    [randy.core :as r]
    [selmer.parser :as selmer]
    [selmer.util :as su]))

;TODO move randoms to companion app, integrate into :data plugin, remove methodical, use namespaced keyword for random
(m/defmulti ^:private randoms-preset (fn [s _] s))

(def ^:private ^:dynamic *rng* @r/default-rng)

(defn- sample-fn [vs]
  (fn sample-random [] (r/sample *rng* vs)))

(defn- preset->fn [preset args]
  (let [v (randoms-preset preset args)]
    (cond-> v
            (vector? v) sample-fn)))

(defn- randoms-filter [_ preset & args]
  ((preset->fn (keyword preset) args)))

(selmer/add-filter! :random randoms-filter)

(defn sample-preset [rng preset & args]
  (r/sample rng (randoms-preset preset args)))

(defn- regurgitate-missing [{:keys [tag-value]} _opts]
  (if tag-value (str "{{" tag-value "}}") ""))

(defn render-randoms [template rng]
  (binding [*rng* rng
            su/*escape-variables*         false
            su/*missing-value-formatter*  regurgitate-missing]
    (selmer/render template {:selmer.filter-parser/selmer-safe-filter true})))

(m/defmethod randoms-preset :feats [_ _]
  ["Alert" "Athlete" "Blinktouched" "Brawler" "Charger" "Crippler" "Cruel" "Decayer" "Defensive Duelist"
   "Dual Wielder" "Dungeon Delver" "Durable" "Eldritch Adept" "Fighting Initiate" "Grappler" "Great Weapon Master"
   "Healer" "Heavy Armour Master" "Inspiring Leader" "Keen Mind" "Light Armour Master" "Magic Initiate"
   "Martial Scholar" "Master Traveler" "Medium Armour Master" "Metamagic Adept" "Mounted Combatant" "Reflective"
   "Resilient" "Sentinel" "Sharpshooter" "Shield Master" "Skilled" "Skulker" "Socialite" "Specialist" "Spell Touched"
   "Summoner" "Survivor" "Tactician" "Telekinetic" "War Caster" "Warlord"])

(m/defmethod randoms-preset :skills [_ _]
  ["Athletics" "Brawn"
   "Finesse" "Stealth"
   "History" "Magiscience" "Medicine" "Nature"
   "Insight" "Investigation" "Perception" "Survival"
   "Deception" "Intimidation" "Persuasion"])

(m/defmethod randoms-preset :damage-types [_ [type]]
  (case (or type "all")
    "physical" ["bludgeoning" "piercing" "slashing"]
    "non-physical" ["acid" "cold" "fire" "force" "lightning" "necrotic" "poison" "psychic" "radiant" "thunder"]
    "all" ["acid" "bludgeoning" "cold" "fire" "force" "lightning" "necrotic" "piercing" "poison" "psychic" "radiant" "slashing" "thunder"]))

(m/defmethod randoms-preset :ability-scores [_ _]
  ["Charisma" "Dexterity" "Intelligence" "Strength" "Wisdom"])

(m/defmethod randoms-preset :monster-types [_ _]
  ["Abberation" "Beast" "Celestial" "Construct" "Dragon" "Elemental" "Fey"
   "Fiend" "Giant" "Humanoid" "Monstrosity" "Ooze" "Plant" "Undead"])

(m/defmethod randoms-preset :cantrips [_ _]
  ["Acid Splash" "Arcane Muscles" "Blade Ward" "Booming Blade" "Chill Touch" "Create Bonfire" "Fire Bolt"
   "Green-Flame Blade" "Gust" "Infestation" "Light" "Lightning Lure" "Magic Stone" "Pestilence" "Primal Savagery"
   "Ray of Frost" "Resistance" "Sacred Flame" "Shocking Grasp" "Spare the Dying" "Tend Wounds" "Thunderclap"
   "Toll the Dead" "Toxic Coating" "True Strike" "Vicious Mockery"])

(m/defmethod randoms-preset :weapon-categories [_ _]
  ["Axe" "Bow" "Brawling" "Caster" "Club" "Dart" "Flail" "Hammer" "Knife" "Pick" "Polearm" "Sling" "Spear" "Sword" "Targe" "Trap"])

(m/defmethod randoms-preset :defences [_ [type]]
  (cond-> ["Fortitude" "Reflexes" "Will"]
          (not= "non-armour" type) (conj "Armour")))

(m/defmethod randoms-preset :conditions [_ _]
  ["blinded" "brittle" "charmed" "confused" "dazed" "deafened" "debilitated" "dominated" "doomed" "fatigue" "fixated"
   "frightened" "grappled" "incapacitated" "poisoned" "prone" "rattled" "retrained" "slowed" "sluggish" "staggered"
   "strife" "taunted" "unconscious" "weakened"])

(m/defmethod randoms-preset :soul-origin [_ _]
  ["otherworldly" "bestial" "celestial" "draconic" "elemental" "fey" "fiendish" "humanoid" "monstrous"])

(m/defmethod randoms-preset :soul-era [_ _]
  ["unborn" "modern" "olden" "ancient" #_"forgotten" "prehistoric" #_"primordial"])

(m/defmethod randoms-preset :literal [_ values] (vec values))

(m/defmethod randoms-preset :without-replacement [_ [amount preset & preset-args]]
  (let [vs (randoms-preset (keyword preset) preset-args)
        amount (parse-long amount)]
    #(r/sample-without-replacement amount vs)))
