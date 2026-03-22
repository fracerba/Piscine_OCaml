let () = 
	let hydrogen = new Atom.hydrogen
	and helium = new Atom.helium
	and lithium = new Atom.lithium
	and beryllium = new Atom.beryllium
	and boron = new Atom.boron
	and carbon = new Atom.carbon
	and nitrogen = new Atom.nitrogen
	and oxygen = new Atom.oxygen
	and fluorine = new Atom.fluorine
	and neon = new Atom.neon
	and sodium = new Atom.sodium
	and magnesium = new Atom.magnesium
	and aluminium = new Atom.aluminium
	and silicon = new Atom.silicon
	and phosphorus = new Atom.phosphorus
	and sulfur = new Atom.sulfur
	and chlorine = new Atom.chlorine
	and argon = new Atom.argon
	and potassium = new Atom.potassium
	and calcium = new Atom.calcium
	and scandium = new Atom.scandium
	and titanium = new Atom.titanium
	and vanadium = new Atom.vanadium
	and chromium = new Atom.chromium
	and manganese = new Atom.manganese
	and iron = new Atom.iron
	and cobalt = new Atom.cobalt
	and nickel = new Atom.nickel
	and copper = new Atom.copper
	and zinc = new Atom.zinc
	and gallium = new Atom.gallium
	and germanium = new Atom.germanium
	and arsenic = new Atom.arsenic
	and selenium = new Atom.selenium
	and bromine = new Atom.bromine
	and krypton = new Atom.krypton
	and rubidium = new Atom.rubidium
	and strontium = new Atom.strontium
	and yttrium = new Atom.yttrium
	and zirconium = new Atom.zirconium
	and niobium = new Atom.niobium
	and molybdenum = new Atom.molybdenum
	and technetium = new Atom.technetium
	and ruthenium = new Atom.ruthenium
	and rhodium = new Atom.rhodium
	and palladium = new Atom.palladium
	and silver = new Atom.silver
	and cadmium = new Atom.cadmium
	and indium = new Atom.indium
	and tin = new Atom.tin
	and antimony = new Atom.antimony
	and tellurium = new Atom.tellurium
	and iodine = new Atom.iodine
	and xenon = new Atom.xenon
	and caesium = new Atom.caesium
	and barium = new Atom.barium
	and lanthanum = new Atom.lanthanum
	and cerium = new Atom.cerium
	and praseodymium = new Atom.praseodymium
	and neodymium = new Atom.neodymium
	and promethium = new Atom.promethium
	and samarium = new Atom.samarium
	and europium = new Atom.europium
	and gadolinium = new Atom.gadolinium
	and terbium = new Atom.terbium
	and dysprosium = new Atom.dysprosium
	and holmium = new Atom.holmium
	and erbium = new Atom.erbium
	and thulium = new Atom.thulium
	and ytterbium = new Atom.ytterbium
	and lutetium = new Atom.lutetium
	and hafnium = new Atom.hafnium
	and tantalum = new Atom.tantalum
	and tungsten = new Atom.tungsten
	and rhenium = new Atom.rhenium
	and osmium = new Atom.osmium
	and iridium = new Atom.iridium
	and platinum = new Atom.platinum
	and gold = new Atom.gold
	and mercury = new Atom.mercury
	and thallium = new Atom.thallium
	and lead = new Atom.lead
	and bismuth = new Atom.bismuth
	and polonium = new Atom.polonium
	and astatine = new Atom.astatine
	and radon = new Atom.radon
	and francium = new Atom.francium
	and radium = new Atom.radium
	and actinium = new Atom.actinium
	and thorium = new Atom.thorium
	and protactinium = new Atom.protactinium
	and uranium = new Atom.uranium
	and neptunium = new Atom.neptunium
	and plutonium = new Atom.plutonium
	and americium = new Atom.americium
	and curium = new Atom.curium
	and berkelium = new Atom.berkelium
	and californium = new Atom.californium
	and einsteinium = new Atom.einsteinium
	and fermium = new Atom.fermium
	and mendelevium = new Atom.mendelevium
	and nobelium = new Atom.nobelium
	and lawrencium = new Atom.lawrencium
	and rutherfordium = new Atom.rutherfordium
	and dubnium = new Atom.dubnium
	and seaborgium = new Atom.seaborgium
	and bohrium = new Atom.bohrium
	and hassium = new Atom.hassium
	and meitnerium = new Atom.meitnerium
	and darmstadtium = new Atom.darmstadtium
	and roentgenium = new Atom.roentgenium
	and copernicium = new Atom.copernicium
	and nihonium = new Atom.nihonium
	and flerovium = new Atom.flerovium
	and moscovium = new Atom.moscovium
	and livermorium = new Atom.livermorium
	and tennessine = new Atom.tennessine
	and oganesson = new Atom.oganesson in
	
	let periodic_table = [hydrogen; helium; 
		lithium; beryllium; boron; carbon; nitrogen; oxygen; fluorine; neon; 
		sodium; magnesium; aluminium; silicon; phosphorus; sulfur; chlorine; argon; 
		potassium; calcium; scandium; titanium; vanadium; chromium; manganese; iron; cobalt; nickel; copper; zinc; gallium; germanium; arsenic; selenium; bromine; krypton; 
		rubidium; strontium; yttrium; zirconium; niobium; molybdenum; technetium; ruthenium; rhodium; palladium; silver; cadmium; indium; tin; antimony; tellurium; iodine; xenon; 
		caesium; barium; lanthanum; cerium; praseodymium; neodymium; promethium; samarium; europium; gadolinium; terbium; dysprosium; holmium; erbium; thulium; ytterbium; lutetium; hafnium; tantalum; tungsten; rhenium; osmium; iridium; platinum; gold; mercury; thallium; lead; bismuth; polonium; astatine; radon; 
		francium; radium; actinium; thorium; protactinium; uranium; neptunium; plutonium; americium; curium; berkelium; californium; einsteinium; fermium; mendelevium; nobelium; lawrencium; rutherfordium; dubnium; seaborgium; bohrium; hassium; meitnerium; darmstadtium; roentgenium; copernicium; nihonium; flerovium; moscovium; livermorium; tennessine; oganesson 
	] in

	List.iter (fun a -> print_endline (a#name ^ " - " ^ a#symbol ^ " - " ^ string_of_int (a#atomic_number))) periodic_table;
	print_newline ();

	List.iter (fun a -> print_endline a#to_string) periodic_table;
	print_newline ();

	print_endline (string_of_bool (hydrogen#equals hydrogen));
	print_endline (string_of_bool (hydrogen#equals (new Atom.hydrogen)));
	print_endline (string_of_bool (hydrogen#equals helium));
	print_newline ();

	let n2 = nitrogen#to_list 2
	and o3 = oxygen#to_list 3 in
	List.iter (fun a -> print_endline a#to_string) n2;
	List.iter (fun a -> print_endline a#to_string) o3;