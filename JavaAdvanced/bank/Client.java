package info.kgeorgiy.ja.sotnikov.bank;

import org.junit.Assert;

import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.util.Arrays;
import java.util.Objects;

public final class Client {

    private static final String BANK = "//localhost/bank";

    /**
     * Utility class.
     */
    public Client() {
    }

//    /**
//     * Debug constructor
//     */
//    public Client(String name, String surname, String passportId, String subId, int amount, Bank bank) throws Exception {
//        goToBank(name, surname, passportId, subId, amount, bank);
//    }

    public static void main(final String... args) throws Exception {
        if (Objects.isNull(args) ||
                args.length != 5 ||
                Arrays.stream(args).anyMatch(Objects::isNull)
        ) {
            throw new IllegalArgumentException("Invalid arguments!");
        }

        int amount;

        String name = args[0];
        String surname = args[1];
        String passportId = args[2];
        String subId = args[3];

        try {
            amount = Integer.parseInt(args[4]);
        } catch (NumberFormatException e) {

            throw new IllegalArgumentException("Invalid balance delta amount!");
        }

        Bank bank = boundBank();

        Assert.assertNotNull(bank);

        new Client().goToBank(name, surname, passportId, subId, amount, bank);
    }

    public void goToBank(String name, String surname, String passportId, String subId, int amount, Bank bank) throws Exception {
        Person person = bank.getRemotePerson(passportId);

        if (Objects.isNull(person)) {
            System.out.println("Creating person...");
            person = bank.createPerson(name, surname, passportId);
        } else {
            System.out.println("Person already registered");

            if (!person.getName().equals(name) || !person.getSurname().equals(surname)) {
                System.out.println("A person with such a passport is registered, but has a different name or surname");
                throw new IllegalArgumentException("Wrong name or surname!");
            }
        }

        Account account = bank.getAccount(person, subId);

        if (Objects.isNull(account)) {
            System.out.println("Creating account...");
            account = bank.createAccount(person, subId);
        } else {
            System.out.println("Account already exist");
        }

        addMoney(account, amount);
    }

    private static void addMoney(Account account, int amountDelta) throws Exception {
        System.out.println("Account id: " + account.getId());
        System.out.println("Money: " + account.getAmount());
        System.out.println("Adding money");
        account.setAmount(account.getAmount() + amountDelta);
        System.out.println("Money: " + account.getAmount());
    }

    private static Bank boundBank() throws RemoteException {
        Bank bank = null;

        try {
            bank = (Bank) Naming.lookup(BANK);
        } catch (final NotBoundException e) {
            System.out.println("Bank is not bound");
        } catch (final MalformedURLException e) {
            System.out.println("Bank URL is invalid");
        }

        return bank;
    }
}
