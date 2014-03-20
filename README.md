# Scrutinator #

Scrutinator is a request binding/validation library with an emphasis on type safety.

## Principles ##

1. As type-safe as possible
    * Optional values without a provided default are Options, other values are not
    * Shouldn't be allowed to request something that hasn't been bound from the request yet
    * Shouldn't be allowed to ask for a type to be bound from the request if there isn't evidence that we can perform that binding
    * When I ask for params("age"), since I have specified the "age" field, the result should be typed to an int
    * When I ask for params("name"), since I have specified the "name" field, the result should be typed to a string
    * When I ask for params("oops"), since I have not specified an "oops" param, I should get a compile error
2. Convenient - should be able to bind multiple values at once and get a Validation of the sequenced individual validations
3. Swagger integration - bindings can be used to generate Swagger specs

## Build & run ##

```sh
$ cd scalatra-shapeless-bindings
$ chmod u+x sbt
$ ./sbt
> +run
```

## Contact ##

- Cody Allen
- <a href="ceedubs@gmail.com">ceedubs@gmail.com</a>
