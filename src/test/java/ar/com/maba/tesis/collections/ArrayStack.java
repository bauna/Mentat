package ar.com.maba.tesis.collections;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import clojure.main;

import ar.com.maba.tesis.preconditions.ClassDefinition;
import ar.com.maba.tesis.preconditions.Pre;

@ClassDefinition(builder="(new ar.com.maba.tesis.collections.ArrayStack 5)",
        invariant = "(>= (:limit vs) (count (:stack vs)))")
public class ArrayStack implements Stack<Integer> {
    private List<Integer> stack = new ArrayList<>();
    private final int limit;
    
    public ArrayStack(int limit) {
        this.limit = limit;
		try {
			Field f;
			f = getClass().getDeclaredField("stack");
			f.getDeclaringClass();
			Class<?> c = (Class<?>) ((ParameterizedType)f.getGenericType()).getActualTypeArguments()[0];
			System.out.println("ñlfk: " + c.getName());
		} catch (NoSuchFieldException | SecurityException e) {
			e.printStackTrace();
		}
    }

    /**
     * @see ar.com.maba.tesis.collections.Stack#push(T)
     */
    @Override
    @Pre(value = "(> (:limit vs) (count (:stack vs)))", data = "\"foo\"")
    public void push(Integer o) {
        if (limit == stack.size()) {
            throw new IllegalStateException("Limit reached");
        }
        stack.add(o);
        if (stack.size() > 3)
        	throw new RuntimeException();
    }
    
    /**
     * @see ar.com.maba.tesis.collections.Stack#pop()
     */
    @Override
    @Pre("(not (empty? (:stack vs)))")
    public Integer pop() {
        if (stack.isEmpty()) {
            throw new IllegalStateException("Stack is empty");
        }
        return stack.remove(stack.size() - 1);
    }
    
    @Override
    @Pre(enabled = false)
    public String toString() {
        return stack.toString();
    }
    
    public static void main(String args[]) {
    	System.out.println(Boolean.TYPE.getName());
    }
}
