# juiceUP Feedback Dashboard

Interní R Shiny dashboard pro sledování workshopové zpětné vazby napříč lektory a klienty. Aplikace při běhu načítá živá data z API, převádí je do jednoho kanonického long-formátu, počítá stávající metriky a zobrazuje dashboard v češtině.

## Lokální spuštění

1. Nainstalujte nebo obnovte balíčky potřebné pro aplikaci a testy.
2. Vytvořte lokální necommitovaný soubor `.Renviron` podle `.Renviron.example`.
3. Spusťte aplikaci z kořene projektu pomocí `shiny::runApp()`.

Lokální konfigurace používá pouze `.Renviron`. Soubor `.env` není součástí doporučeného workflow.

## Povinné proměnné prostředí

- `FEEDBACK_API_BASE_URL`: kořenová URL adresa Feedback API
- `FEEDBACK_API_TOKEN`: bearer token pro autorizaci API volání
- `FORM_ID`: identifikátor formuláře, ze kterého se načítají odpovědi

Aplikace čte odpovědi z cesty `FEEDBACK_API_BASE_URL/forms/FORM_ID/responses`. Lookback okno je nyní pevná konstantní hodnota v kódu.

## Testování

Automatické testy nespouštějí živé API volání a nevyžadují reálný token.

```r
source("tests/testthat.R")
```

Pokud měníte runtime kód nebo závislosti, po dokončení změn znovu vygenerujte deployment manifest:

```r
rsconnect::writeManifest(appPrimaryDoc = "app.R")
```

## Posit Connect Cloud

Do deployment bundle nepatří `.Renviron`, `.env`, lokální knihovna `renv/library/` ani testovací soubory. Na Posit Connect Cloud nastavte `FEEDBACK_API_BASE_URL`, `FEEDBACK_API_TOKEN` a `FORM_ID` v nastavení aplikace a po změně runtime kódu nebo závislostí regenerujte `manifest.json`.

## Bezpečnost

Tajné hodnoty patří jen do lokálního `.Renviron` nebo do Posit Connect Cloud konfigurace. Necommitujte tokeny, lokální konfigurační soubory ani jiné citlivé údaje.

## Aktuální dashboard kontrakt

- Hlavní KPI v dashboardu je pouze `Jak hodnotíte celkově dnešní workshop?`.
- Dashboard už nepoužívá composite score ani response rate.
- Druhou entitou vedle lektorů jsou témata workshopů, ne klienti.
- API vrstva vrací dvě kanonické tabulky: `responses` a `scores_long`.
- `responses` obsahuje jednu řádku na jednu odpověď, včetně tří oddělených textových komentářů.
- `scores_long` obsahuje jen čtyři detailní hodnocené oblasti pro breakdown vizualizace.
