package ar.com.maba.tesis.atm;

import ar.com.maba.tesis.preconditions.ClassDefinition;
import ar.com.maba.tesis.preconditions.Pre;

@ClassDefinition(
	    builder = "(new ar.com.maba.tesis.atm.AtmImpl)", 
	    invariant = "(or cardInside (not authenticated)) ")

public class AtmImpl implements Atm {
	
	private boolean authenticated = false;
	private boolean cardInside = false;
	
	private void checkAuthenticated() {
		if (!authenticated) {
			throw new IllegalStateException("must be authenticated");
		}
	}
	
	private void checkNotAuthenticated() {
		if (authenticated) {
			throw new IllegalStateException("must be not authenticated");
		}
	}
	
	private void checkCardInside() {
		if (!cardInside) {
			throw new IllegalStateException("card must be inserted");
		}
	}
	
	private void checkCardNotInside() {
		if (cardInside) {
			throw new IllegalStateException("card must not be inserted");
		}
	}
	

	@Override
	@Pre(value = "(and cardInside (not authenticated))")
	public void authenticate() {
		checkCardInside();
		checkNotAuthenticated();
		authenticated = true;
	}

	@Override
	@Pre(value = "(authenticated)")
	public void finish() {
		checkAuthenticated();
		authenticated = false;
	}

	@Override
	@Pre(value = "(not cardInside)")
	public void insertCard() {
		checkCardNotInside();
		cardInside = true;
	}

	@Override
	@Pre(value = "(and (not authenticated) cardInside)")
	public void removeCard() {
		checkNotAuthenticated();
		checkCardInside();
		cardInside = false;
	}

	@Override
	@Pre(value = "(authenticated)")
	public void operate() {
		checkAuthenticated();
	}

	@Override
	@Pre(value = "(authenticated)")
	public void printTicket() {
		checkAuthenticated();
	}

}
