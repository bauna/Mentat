package ar.com.maba.tesis.preconditions;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Class annotation for defining the invariant and instance builder. 
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface ClassDefinition {

    /** Defines the invariant expression */
    String invariant() default "true";

    /** Defines an instance creation function */
    String builder();
}
