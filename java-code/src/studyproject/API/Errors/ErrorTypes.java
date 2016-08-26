package studyproject.API.Errors;

import java.util.HashMap;
import java.util.Map;

public enum ErrorTypes {
	IO, blub, blah;
	

    private static Map<Integer, ErrorTypes> map = new HashMap<Integer, ErrorTypes>();

    static {
        for (ErrorTypes error : ErrorTypes.values()) {
            map.put(error.ordinal(), error);
        }
    }

    public static ErrorTypes valueOf(int error) {
        return map.get(error);
    }
}
