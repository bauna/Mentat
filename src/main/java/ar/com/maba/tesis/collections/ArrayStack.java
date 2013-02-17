package ar.com.maba.tesis.collections;

import java.util.ArrayList;
import java.util.List;

import ar.com.maba.tesis.preconditions.Pre;

public class ArrayStack<T> implements Stack<T> {
    private List<T> stack = new ArrayList<>();
    private final int limit;
    
    public ArrayStack(int limit) {
        this.limit = limit;
    }

    /**
     * @see ar.com.maba.tesis.collections.Stack#push(T)
     */
    @Override
    @Pre("true")
    public void push(T o) {
        if (limit == stack.size()) {
            throw new IllegalStateException("Limit reached");
        }
        stack.add(o);
    }
    
    /**
     * @see ar.com.maba.tesis.collections.Stack#pop()
     */
    @Override
    @Pre("true")
    public T pop() {
        if (stack.isEmpty()) {
            throw new IllegalStateException("Stack is empty");
        }
        return stack.remove(stack.size() - 1);
    }
    
    @Override
    @Pre("false")
    public String toString() {
        return stack.toString();
    }
}
