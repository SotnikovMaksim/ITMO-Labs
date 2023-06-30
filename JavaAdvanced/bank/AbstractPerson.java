package info.kgeorgiy.ja.sotnikov.bank;

import java.rmi.RemoteException;

/**
 * Abstract implementation of {@link Person} interface, providing the common methods and properties.
 * This class defines the common properties like name, surname, and passportId of a person.
 * The methods for account management are left to be implemented by subclasses.
 *
 * @see Person
 */
public abstract class AbstractPerson implements Person {
    private final String name;
    private final String surname;
    private final String passportId;
    final Bank bank;

    public AbstractPerson(String name, String surname, String passportId, Bank bank) {
        this.passportId = passportId;
        this.surname = surname;
        this.name = name;
        this.bank = bank;
    }

    /**
     * Retrieves the name of this person.
     *
     * @return the name of this person.
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * Retrieves the surname of this person.
     *
     * @return the surname of this person.
     */
    @Override
    public String getSurname() {
        return surname;
    }

    /**
     * Retrieves the passport id of this person.
     *
     * @return the passport id of this person.
     */
    @Override
    public String getPassportId() {
        return passportId;
    }

    /**
     * Retrieves the account associated with the specified subId for this person.
     *
     * @param subId the sub-identifier of an account.
     * @return the account associated with the specified subId.
     * @throws RemoteException if a RemoteException occurs during execution.
     */
    @Override
    public Account getAccount(String subId) throws RemoteException {
        return getAccountImpl(subId);
    }

    /**
     * Creates a new account associated with the specified subId for this person.
     *
     * @param subId the sub-identifier of an account.
     * @return the newly created account.
     * @throws RemoteException if a RemoteException occurs during execution.
     */
    @Override
    public Account createAccount(String subId) throws RemoteException {
        return createAccountImpl(subId);
    }

    public abstract Account createAccountImpl(String subId) throws RemoteException;

    public abstract Account getAccountImpl(String subId) throws RemoteException;
}
