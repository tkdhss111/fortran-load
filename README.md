# fortran-load 

Server Program for Making Electricity Load Database

- Collects electricy load data hourly from TEPCO PG [でんき予報](https://www.tepco.co.jp/forecast)

# Dependencies

- fortran-logger
- fortran-datetime
- fortran-sqlite3

# Install

```
cd install
sudo make service
```

# Uninstall

```
cd install
sudo make stop
sudo make uninstall
```
