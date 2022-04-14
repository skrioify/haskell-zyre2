# Changelog for zyre2

## 0.1.0.3

Initial release. Versions 0.1.0.0 through 0.1.0.2 are internal development releases.
Developed and tested with zyre release 2.0.1.

Supports most functionality of zyre, excluding:

 * Draft API
 * Gossip protocol discovery

The following zyre functions are excluded:

 * int zyre_set_endpoint (zyre_t *self, const char *format, ...) CHECK_PRINTF (2);
 * void zyre_gossip_bind (zyre_t *self, const char *format, ...) CHECK_PRINTF (2);
 * void zyre_gossip_connect (zyre_t *self, const char *format, ...) CHECK_PRINTF (2);