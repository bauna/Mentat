package ar.com.maba.tesis.collections;

import java.util.ArrayList;
import java.util.List;

import ar.com.maba.tesis.preconditions.ClassDefinition;
import ar.com.maba.tesis.preconditions.Pre;

@ClassDefinition(
    builder = "(new ar.com.maba.tesis.collections.ArrayStack 1)", 
    invariant = "(>= limit (count stack))")
public class ArrayStack implements Stack<Integer> {
    private List<Integer> stack = new ArrayList<>();
    private final int limit;

    public ArrayStack(int limit) {
        this.limit = limit;
    }

    /**
     * @see ar.com.maba.tesis.collections.Stack#push(T)
     */
    @Override
//    @Pre(value = "(and (> p0 5) (> limit (eval (count stack))))")
    @Pre(value = "(> limit (eval (count stack)))", data = "{:p0 (count stack)}")
    public void push(Integer n) {
        if (limit == stack.size()) {
            throw new IllegalStateException("Limit reached: " + limit);
        }
        stack.add(n);
    }

    /**
     * @see ar.com.maba.tesis.collections.Stack#pop()
     */
    @Override
    @Pre(value = "(< 0 (eval (count stack)))")
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
}
