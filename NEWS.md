# hydrotoolbox v 1.1.1

### Minor changes
* Package documentation updated.

* `interpolate` function now accepts `tibbles`.

* Informative comment added to `hm_report` and `report_miss`
functions.


### Major changes (v 1.1.0)
* The `hydromet_*-classes` now accepts `tibbles`. Also, the 
column classes of the slots could be any (and not just numeric).

* `hm_build_generic()` method has been created. The function allows
for the creation of an `hydromet_station-class` using generic 
raw files.

* The `hydromet_station` class has new slots: outgoing longwave
and shortwave radiation.
