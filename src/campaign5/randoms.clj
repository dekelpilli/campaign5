(ns campaign5.randoms
  (:require
    [campaign5.util :as u]
    [methodical.core :as m]
    [randy.core :as r]
    [selmer.parser :as selmer]))

(m/defmulti ^:private randoms-factor (fn [s _] (keyword s)))
(m/defmulti ^:private randoms-preset (fn [s _] s))

(defn- sample-fn [vs]
  (fn sample-random [] (r/sample vs)))

(defn- preset->fn [preset args]
  (let [v (randoms-preset preset args)]
    (cond-> v
            (vector? v) sample-fn)))

(defn- randoms-filter [_ preset & args]
  ((preset->fn (keyword preset) args)))

(selmer/add-filter! :random randoms-filter)

(defn- calculate-template-weightings [template]
  (let [weighting (transduce
                    (comp (keep (comp u/extract-format-tags :tag-value :tag meta))
                          (filter (comp #{"x|random"} first))
                          (map (fn [[_ preset & args]] (randoms-factor preset args))))
                    +
                    0
                    template)]
    (max 1 weighting)))

(defn attach-weightings [{:keys [template weighting] :as mod}]
  (cond-> mod
          (nil? weighting) (assoc :weighting (calculate-template-weightings template))))

(m/defmethod randoms-factor :languages [_] 1)
(m/defmethod randoms-preset :languages [_ _]
  ["Common" "Dwarvish" "Elvish" "Giant" "Gnomish" "Goblin" "Halfling" "Orc"
   "Abyssal" "Celestial" "Draconic" "Deep Speech" "Infernal" "Primordial" "Sylvan" "Undercommon"])

(m/defmethod randoms-factor :feats [_ _] 1)
(m/defmethod randoms-preset :feats [_ _]
  ["Alert" "Athlete" "Blinktouched" "Brawler" "Charger" "Crippler" "Cruel" "Decayer" "Defensive Duelist"
   "Dual Wielder" "Dungeon Delver" "Durable" "Eldritch Adept" "Fighting Initiate" "Grappler" "Great Weapon Master"
   "Healer" "Heavy Armour Master" "Inspiring Leader" "Keen Mind" "Light Armour Master" "Magic Initiate"
   "Martial Scholar" "Master Traveler" "Medium Armour Master" "Metamagic Adept" "Mounted Combatant" "Reflective"
   "Resilient" "Sentinel" "Sharpshooter" "Shield Master" "Skilled" "Skulker" "Socialite" "Specialist" "Spell Touched"
   "Summoner" "Survivor" "Tactician" "Telekinetic" "War Caster" "Warlord"])

(m/defmethod randoms-factor :skills [_ _] 4)
(m/defmethod randoms-preset :skills [_ _]
  ["Athletics" "Brawn"
   "Finesse" "Stealth"
   "History" "Magiscience" "Medicine" "Nature"
   "Insight" "Investigation" "Perception" "Survival"
   "Deception" "Intimidation" "Persuasion"])

(m/defmethod randoms-factor :damage-types [_ [type]]
  (case (or type "all")
    "physical" 1
    "non-physical" 2
    "all" 3))
(m/defmethod randoms-preset :damage-types [_ [type]]
  (case (or type "all")
    "physical" ["bludgeoning" "piercing" "slashing"]
    "non-physical" ["acid" "cold" "fire" "force" "lightning" "necrotic" "poison" "psychic" "radiant" "thunder"]
    "all" ["acid" "bludgeoning" "cold" "fire" "force" "lightning" "necrotic" "piercing" "poison" "psychic" "radiant" "slashing" "thunder"]))

(m/defmethod randoms-factor :ability-scores [_ _] 3)
(m/defmethod randoms-preset :ability-scores [_ _]
  ["Charisma" "Dexterity" "Intelligence" "Strength" "Wisdom"])

(m/defmethod randoms-factor :monster-types [_ _] 2)
(m/defmethod randoms-preset :monster-types [_ _]
  ["Abberation" "Beast" "Celestial" "Construct" "Dragon" "Elemental" "Fey"
   "Fiend" "Giant" "Humanoid" "Monstrosity" "Ooze" "Plant" "Undead"])

(m/defmethod randoms-factor :cantrips [_ _] 2)
(m/defmethod randoms-preset :cantrips [_ _]
  ["Acid Splash" "Arcane Muscles" "Blade Ward" "Booming Blade" "Chill Touch" "Create Bonfire" "Fire Bolt"
   "Green-Flame Blade" "Gust" "Infestation" "Light" "Lightning Lure" "Magic Stone" "Pestilence" "Primal Savagery"
   "Ray of Frost" "Resistance" "Sacred Flame" "Shocking Grasp" "Spare the Dying" "Tend Wounds" "Thunderclap"
   "Toll the Dead" "Toxic Coating" "True Strike" "Vicious Mockery"])

(m/defmethod randoms-factor :weapon-categories [_ _] 2)
(m/defmethod randoms-preset :weapon-categories [_ _]
  ["Axe" "Bow" "Brawling" "Caster" "Club" "Dart" "Flail" "Hammer" "Knife" "Pick" "Polearm" "Sling" "Spear" "Sword" "Targe" "Trap"])

(m/defmethod randoms-factor :defences [_ [type]]
  (cond-> 3
          (not= "non-armour" type) inc))
(m/defmethod randoms-preset :defences [_ [type]]
  (cond-> ["Fortitude" "Reflexes" "Will"]
          (not= "non-armour" type) (conj "Armour")))

(m/defmethod randoms-factor :conditions [_ _] 2)
(m/defmethod randoms-preset :conditions [_ _]
  ["blinded" "brittle" "charmed" "confused" "dazed" "deafened" "debilitated" "dominated" "doomed" "fatigue" "fixated"
   "frightened" "grappled" "incapacitated" "poisoned" "prone" "rattled" "retrained" "slowed" "sluggish" "staggered"
   "strife" "taunted" "unconscious" "weakened"])

(m/defmethod randoms-factor :literal [_ _] 1)
(m/defmethod randoms-preset :literal [_ values] (vec values))

(m/defmethod randoms-factor :without-replacement [_ {:keys [amount from]}] (max amount (randoms-factor from)))
(m/defmethod randoms-preset :without-replacement [_ [amount preset & preset-args]]
  (let [vs (randoms-preset (keyword preset) preset-args)
        amount (parse-long amount)]
    #(r/sample-without-replacement amount vs)))
