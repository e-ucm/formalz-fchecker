## Welcome

This is our webservice's API.

Enjoy!

## POST /compare

#### Request:
- Supported content types are:
    - `application/json;charset=utf-8`
    - `application/json`

```javascript
{
  // teacher's specification
  "sourceA":
    "public static float real1(float a) { \
       pre(a >= (2 - 1 + 1)); \
       a += a; \
       post(a >= (4 - 3 + 3)); \
     }",
  // student's specification
  "sourceB":
    "public static float real2(float a) { \
       pre(a > 2 || a == 2); \
       a = a * 2; \
       post(a > 4 || a == 4); \
     }"
  // optional control of which conditions to check
  "options": {"checkPre": true, "checkPost": false}
}
```

#### Response:

- Status code 200
- Headers: []
- Supported content types are:
    - `application/json;charset=utf-8`
    - `application/json`


The JSON response has the following format:
```javascript
{ "responseType": /* are the specification equivalent*/,
  /* in case of non-equivalence, we give a counter-model */,
  "model": {
    <var1>: <value1>
    <var2>: <value2>
    ...
  }
  /* in case of non-equivalence, we give all possible truth value-pairs of the specifications */
  "feedback": {
    "pre":  [/*both true*/, /*true, false*/, /*false, true*/, /*both false*/],
    "post": [/*both true*/, /*true, false*/, /*false, true*/, /*both false*/]
  }
  "err": /* an error occurred! */
}
```

There are 3 possible responses:
  1. The specification are equivalent:
```javascript
{ "responseType": "Equiv",
       "model": null,
       "feedback": null,
       "err": null
}
```
  2. The specification are not equivalent:
```javascript
{ "responseType": "NotEquiv",
       "model": {"x": -10},
       "feedback": {
         "pre": [true, false, false, false],
         "post": [true, true, false, false]
       },
       "err": null
}
```
  3. An error occurred:
```javascript
{ "responseType": "ResponseErr",
       "err": "(==): heterogeneous types",
       "model": null.
       "feedback": null
}
```
