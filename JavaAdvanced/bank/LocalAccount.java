package info.kgeorgiy.ja.sotnikov.bank;

import java.io.Serializable;

public class LocalAccount extends AbstractAccount implements Serializable {

    public LocalAccount(String id) {
        super(id);
    }
}
