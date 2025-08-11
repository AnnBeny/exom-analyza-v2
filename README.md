# CNV Analýza Exomy rozšíření

Interaktivní nástroj v R Shiny pro rychlý screening a kontrolu CNV odchylek (Copy Number Variations) z exomového sekvenování. Umožňuje:
- nahrát více souborů *.coveragefin.txt* (1 soubor = 1 vzorek),
- přiřadit pohlaví ke každému vzorku (M/Ž) a zvolit rozsahy/oblasti k vyloučení,
- filtrovat podle chromozomu X (vše / bez X / pouze X),
- zobrazit tabulky **Coverage Mean ALL**, **CNV Muži Mean**, **CNV Ženy Mean**,
- stáhnout výsledky do CSV,
- anotovat podezřelé regiony z **OMIM**.

> Limit pro upload: 30 MB na dávku (`options(shiny.maxRequestSize = 30 * 1024^2)`).

---

## Struktura projektu
exom_analyza_cktch/
├── app/ # app.R / ui, server, helpers.R
├── reference/ # OMIM referenční soubor
├── CNV_exom.desktop # Spouštěcí soubor (Linux)
├── launch.sh # Spouštěcí skript (Linux)
├── launch_app.bat # Spouštěcí skript (Windows)
├── icon_exom2.png # Ikona
└── icon_exom2.ico # Ikona (Windows)

---

## Požadavky
- **R ≥ 4.1**
- R balíčky: `shiny`, `bslib`, `DT`, `magrittr`
  - instalace: `install.packages(c("shiny", "bslib", "DT", "magrittr"))`

---

## Formát vstupu
Soubory s příponou **.coveragefin.txt**, tabulátorově oddělené, sloupce v pořadí:
1. `chr`
2. `start`
3. `stop`
4. `name` (gen/region)
5. `COV-mean` (průměrné pokrytí)
6. `COV-procento` (procentuální pokrytí)

> Název vzorku se bere z názvu souboru (bez sufixu `.coveragefin.txt`).

---

## Jak aplikaci spustit
- **Windows:** zástupce napojený na `launch_app.bat`
- **Linux:** zástupce/ikona napojená na `launch.sh`

Anebo přímo v R: `shiny::runApp()` ve složce aplikace.

---

## Postup práce (UI)
1. **Nahrát soubory** `.coveragefin.txt` (lze víc najednou).
2. **Pohlaví vzorků:** u každého vzorku vybrat *Muž/Žena*.
3. **Filtry (volitelné):**
   - **Oblasti (geny) k vyloučení:** předvyplněno seznamem z dat + výchozí `IKBKG, CFHR, FCGR3A, FCGR3B, FANCD2, IGHG2`. Seznam je možné upravit/rozšířit.
   - **Start/Stop pozice:** lze zadat/rozšířit hodnoty; vybrané pozice budou z analýzy vyloučeny.
   - **Chromozom X:** *Všechny*, *Bez X*, nebo *Pouze X*.
4. **Zpracovat** (zelené tlačítko).  
5. **Záložky s výsledky:**
   - **Coverage Mean ALL:** surové průměrné pokrytí všech vzorků (5. sloupec).
   - **CNV Muži Mean / CNV Ženy Mean:** normalizované pokrytí; zobrazeny řádky, kde aspoň jeden vzorek překročí práh **±0,25** (absolutní hodnota). Výsledek je anotován z **OMIM**.
6. **Stažení výstupů:** tlačítky v levém panelu (CSV).

> Pozn.: Tabulky s *procentuálním pokrytím* (6. sloupec) jsou v kódu připravené, ale v UI jsou aktuálně skryté. Lze je později zapnout (taby/btn pro `*_proc`).

---

## Výstupy (CSV)
- **coveragemeanALL.csv** – spojení pozic + průměrné pokrytí všech vzorků.
- **CNV_M_mean.csv** – normalizované hodnoty pro mužské vzorky (jen řádky s vychýlením > 0,25; vč. OMIM).
- **CNV_Z_mean.csv** – totéž pro ženské vzorky.
- *(volitelné, nyní vypnuto v UI)*: **coverageprocentoALL.csv**, **CNV_M_procento.csv**, **CNV_Z_procento.csv** – surová procenta pokrytí.

---

## OMIM reference
- Umístění: `reference/` (např. `omim-phenptype-2024-upr-sl67.txt`).
- Formát: tab-delimited; 1. sloupec = gen, další = fenotypy.
- Načtení zajišťuje `load_omim_file()`; anotace provádí `annotate_with_omim()`.

---

## Poznámky k filtrování
- **Geny/oblasti:** vybrané názvy se z výsledků **vyloučí** (regexově – zahrnuje i varianty názvů s příponami).
- **Start/Stop:** řádky s pozicemi v uvedených seznamech se **vyloučí**.
- **Chromozom X:** přepínač ovlivní všechny tabulky (ALL/CNV i procenta).

---

## Časté potíže a tipy
- **Prázdné tabulky CNV:** zkontrolujte, zda některé normalizované hodnoty přesahují **±0,25**; jinak se nic nezobrazí.
- **Nesprávné sloupce:** vstup musí mít přesně 6 sloupců v uvedeném pořadí.
- **Chybí OMIM anotace:** ověřte cestu k referenčnímu souboru a kódování/oddělovače.
- **Názvy chromozomů:** skript odstraňuje u `chr` uvozovky; formát vstupu by měl být konzistentní.

