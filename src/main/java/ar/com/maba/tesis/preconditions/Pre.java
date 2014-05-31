package ar.com.maba.tesis.preconditions;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 *  Annotation for defining a method precondition.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Pre {
    /** Defines the precondition expression */
    String value() default "true";

    /** Defines a parameter definition function */
    String data() default "";

    /** Defines if this method should be included in the analysis */
    boolean enabled() default true;

    /** Defines an alternative name for the method */
    String name() default "";
}
