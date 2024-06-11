package info.kgeorgiy.ja.sotnikov.bank;

public class AbstractAccount implements Account {

    private final String id;
    private int amount;

    public AbstractAccount(final String id) {
        this.amount = 0;
        this.id = id;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public synchronized int getAmount() {
        System.out.println("Getting amount of money for account " + id);
        return amount;
    }

    @Override
    public synchronized void setAmount(final int amount) {
        System.out.println("Setting amount of money for account " + id);
        this.amount = amount;
    }
}
