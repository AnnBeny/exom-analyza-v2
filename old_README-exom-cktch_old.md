# Exom Analýza – CNV Screening Tool
Jde o interaktivní nástroj vytvořený v prostředí R Shiny pro rychlý screening a vizuální kontrolu CNV odchylek (Copy Number Variations) ve vzorcích z exomového sekvenování. Aplikace je určena pro odborníky, kteří potřebují:
- porovnat pokrytí genů mezi jednotlivými vzorky,
- detekovat odchylky v pokrytí (CNV) u mužů a žen,
- zobrazit a exportovat výsledky v přehledné tabulkové formě,
- propojit nalezené odchylky s anotacemi z databáze OMIM.
Uživatel nahraje zpracované soubory .coveragefin.txt, vybere pohlaví ke každému vzorku a spustí analýzu. Výsledkem je vizuální zobrazení genových regionů s podezřelými CNV změnami, které jsou doplněny o případné fenotypy z OMIM.

## Struktura složky
exom_analyza_ckcht/
├── app/ # Vlastní Shiny aplikace (ui, server, helpers)
├── data_input/ # Sem se mohou vkládat (ale nemusí) pro přehlednost vstupní .txt soubory (jeden soubor = jeden vzorek). Ale input souborů pracuje nezávisle na ní.
├── reference/ # OMIM referenční soubor (omim-phenptype-2024-upr-sl67.txt)
├── CNV_exom.desktop # Spouštěcí soubor pro Linux (ikona aplikace)
├── launch.sh # Spouštěcí skript (Linux)
├── launch_app.bat # Spouštěcí skript (Windows)
├── icon_exom2.png # Ikona aplikace (grafika)
├── icon_exom2.ico # Ikona pro Windows zástupce
└── README.md # Tento popis projektu

## Požadavky
- **R (verze ≥ 4.1)**
- Nainstalované R balíčky:
  - `shiny`, `bslib`, `DT`, `magrittr`
Instalace např. v R:
install.packages(c("shiny", "bslib", "DT", "magrittr"))

## Spuštění
přes ikonu napojenou na launch_app.bat (Win)
přes ikonu napojenou na launch.sh (Linux)

## Formát vstupních souborů
Přípona: .coveragefin.txt
Každý soubor obsahuje pokrytí jednoho vzorku
Sloupce:
chr, start, stop, name, COV-mean (5. sloupec), COV-procento (6. sloupec)

## Funkce aplikace
Výběr více vstupních souborů najednou
Manuální výběr pohlaví pro každý vzorek (důležité pro analýzu CNV)
Vizualizace:
- Coverage Mean ALL: nenormalizované pokrytí všech vzorků
- CNV Muži Mean: normalizované pokrytí pro mužské vzorky (vč. OMIM anotace)
- CNV Ženy Mean: totéž pro ženské vzorky
Export dat do .csv
Odkaz na databázi OMIM

## OMIM Reference
Soubor omim-phenptype-2024-upr-sl67.txt v reference/
Formát: tab-delimited, první sloupec = gen, další sloupce = fenotypy
Používá se pro anotaci výsledků

## Kontakt
Vytvořeno pro interní použití laboratoře CKCHT.
Pro dotazy nebo úpravy pište na benyskova.anna@fnbrno.cz.
