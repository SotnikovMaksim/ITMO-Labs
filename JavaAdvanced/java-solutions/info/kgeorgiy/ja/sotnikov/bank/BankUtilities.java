package info.kgeorgiy.ja.sotnikov.bank;

import java.rmi.RemoteException;

public class BankUtilities {

    @FunctionalInterface
    public interface ThrowingSupplier<T> {
        T get() throws RemoteException;
    }

    public static <T> T callMethod(ThrowingSupplier<T> supplier) throws RemoteException {
        return supplier.get();
    }
}
