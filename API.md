## Welcome

This is our webservice's API.

Enjoy!

## GET /compare

Clients must supply the following data

#### GET Parameters:

- a
  - **Values**: *examples/javawlp_edsl/src/nl/uu/javawlp_edsl/Main.java:real1, ...*
  - **Description**: Method location formatted as: < java_source_file>:< method_name>

- b
  - **Values**: *examples/javawlp_edsl/src/nl/uu/javawlp_edsl/Main.java:real2, ...*
  - **Description**: Method location formatted as: < java_source_file>:< method_name>

#### Response:

- Status code 200
- Headers: []

```javascript
{ "model": {
    "a": [0, -0.5],
    "i": 10
  },
  "responseType": "NotEquivalent"
}
```
