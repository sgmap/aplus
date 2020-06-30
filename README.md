# Administration+
- Conditions Générales d’Utilisation : https://docs.aplus.beta.gouv.fr/conditions-generales-dutilisation
- Fiche produit : https://beta.gouv.fr/startups/aplus.html
- Statistiques d’usage : https://infogram.com/stats-dusage-dadministration-1hmr6gm9mk5o6nl?live

# Code
- [Guidelines](docs/guidelines.md)
- [Wiki](https://github.com/betagouv/aplus/wiki) (Le wiki est visé à être remplacer par la doc dans le repo)

# Commande

- Lancer une base de donnée Postgresql avec docker-compose :
`docker-compose up db`
- Lancer une serveur Play de dev avec docker-compose :
  `docker-compose up web`
- Pour lancer le serveur sans docker `sbt run` (Vous pouvez regarder les variables d'environnement indispensables dans le `docker-compose.yml` et la liste des variables dans le `application.conf`)

TODO: documentation: pas de package-lock.json, commandes `npm run watch` `npm run clean` `npm run build`
