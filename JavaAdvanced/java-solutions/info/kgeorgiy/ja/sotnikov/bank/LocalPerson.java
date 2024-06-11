package info.kgeorgiy.ja.sotnikov.bank;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.Map;

/**
 * A {@link Person} implementation that manages local accounts.
 *
 * @see AbstractPerson
 * @see Person
 */
public class LocalPerson extends AbstractPerson implements Serializable {

    private final Map<String, Account> accounts;

    public LocalPerson(String name, String surname, String passportId,
                       Bank bank, Map<String, Account> accounts
    ) {
        super(name, surname, passportId, null);
        this.accounts = accounts;
    }

    /**
     * Creates a new local account associated with the specified subId for this person.
     *
     * @param subId the sub-identifier of an account.
     * @return the newly created account.
     * @throws RemoteException if a RemoteException occurs during execution.
     */
    @Override
    public Account createAccountImpl(String subId) throws RemoteException {
        Account account = new LocalAccount(subId);
        return accounts.put(createAccountId(getPassportId(), subId), account);
    }

    /**
     * Retrieves the local account associated with the specified subId for this person.
     *
     * @param subId the sub-identifier of an account.
     * @return the account associated with the specified subId.
     */
    @Override
    public Account getAccountImpl(String subId) {
        return accounts.get(getPassportId() + ":" + subId);
    }

    private static String createAccountId(String passportId, String subId) {
        return passportId + ":" + subId;
    }
}
