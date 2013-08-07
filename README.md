# Scalatra Shapeless Bindings #

## Goals ##

1. As type-safe as possible
    * Optional values without a provided default are Options, other values are not
    * Shouldn't be allowed to request something that hasn't been bound from the request yet
    * Shouldn't be allowed to ask for a type to be bound from the request if there isn't evidence that we can perform that binding
2. Convenient - should be able to bind multiple values at once and get a Validation of the sequenced individual validations
3. Swagger integration - bindings can be used to generate Swagger specs
4. (Maybe) - It would be nice to be able to grab values by field in a type-safe way as opposed to depending on the ordering of fields in a tuple

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
