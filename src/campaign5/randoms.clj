(ns campaign5.randoms
  (:require
    [sns.sdk.randoms :as randoms]))

(defmethod randoms/preset :feats [_ _]
  ["Alert" "Athlete" "Blinktouched" "Brawler" "Charger" "Crippler" "Cruel" "Decayer" "Defensive Duelist"
   "Dual Wielder" "Dungeon Delver" "Durable" "Eldritch Adept" "Fighting Initiate" "Grappler" "Great Weapon Master"
   "Healer" "Heavy Armour Master" "Inspiring Leader" "Keen Mind" "Light Armour Master" "Magic Initiate"
   "Martial Scholar" "Master Traveler" "Medium Armour Master" "Metamagic Adept" "Mounted Combatant" "Reflective"
   "Resilient" "Sentinel" "Sharpshooter" "Shield Master" "Skilled" "Skulker" "Socialite" "Specialist" "Spell Touched"
   "Summoner" "Survivor" "Tactician" "Telekinetic" "War Caster" "Warlord"])

(defmethod randoms/preset :skills [_ _]
  ["Athletics" "Brawn"
   "Finesse" "Stealth"
   "History" "Magiscience" "Medicine" "Nature"
   "Insight" "Investigation" "Perception" "Survival"
   "Deception" "Intimidation" "Persuasion"])

(defmethod randoms/preset :damage-types [_ [type]]
  (case (or type "all")
    "physical" ["bludgeoning" "piercing" "slashing"]
    "non-physical" ["acid" "cold" "fire" "force" "lightning" "necrotic" "poison" "psychic" "radiant" "thunder"]
    "all" ["acid" "bludgeoning" "cold" "fire" "force" "lightning" "necrotic" "piercing" "poison" "psychic" "radiant" "slashing" "thunder"]))

(defmethod randoms/preset :ability-scores [_ _]
  ["Charisma" "Dexterity" "Intelligence" "Strength" "Wisdom"])

(defmethod randoms/preset :monster-types [_ _]
  ["Abberation" "Beast" "Celestial" "Construct" "Dragon" "Elemental" "Fey"
   "Fiend" "Giant" "Humanoid" "Monstrosity" "Ooze" "Plant" "Undead"])

(defmethod randoms/preset :cantrips [_ _]
  ["Acid Splash" "Arcane Muscles" "Blade Ward" "Booming Blade" "Chill Touch" "Create Bonfire" "Fire Bolt"
   "Green-Flame Blade" "Gust" "Infestation" "Light" "Lightning Lure" "Magic Stone" "Pestilence" "Primal Savagery"
   "Ray of Frost" "Resistance" "Sacred Flame" "Shocking Grasp" "Spare the Dying" "Tend Wounds" "Thunderclap"
   "Toll the Dead" "Toxic Coating" "True Strike" "Vicious Mockery"])

(defmethod randoms/preset :weapon-categories [_ _]
  ["Axe" "Bow" "Brawling" "Caster" "Club" "Dart" "Flail" "Hammer" "Knife" "Pick" "Polearm" "Sling" "Spear" "Sword" "Targe" "Trap"])

(defmethod randoms/preset :defences [_ [type]]
  (cond-> ["Fortitude" "Reflexes" "Will"]
          (not= "non-armour" type) (conj "Armour")))

(defmethod randoms/preset :conditions [_ _]
  ["blinded" "brittle" "charmed" "confused" "dazed" "deafened" "debilitated" "dominated" "doomed" "fatigue" "fixated"
   "frightened" "grappled" "incapacitated" "poisoned" "prone" "rattled" "retrained" "slowed" "sluggish" "staggered"
   "strife" "taunted" "unconscious" "weakened"])

(defmethod randoms/preset :soul-origin [_ _]
  ["otherworldly" "bestial" "celestial" "draconic" "elemental" "fey" "fiendish" "humanoid" "monstrous"])

(defmethod randoms/preset :soul-era [_ _]
  ["unborn" "modern" "olden" "ancient" #_"forgotten" "prehistoric" #_"primordial"])
