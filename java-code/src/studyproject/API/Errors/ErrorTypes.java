package studyproject.API.Errors;

import java.util.HashMap;
import java.util.Map;

/**
 * Enum which describes an error and got an internal mapping of enum.ordinal()
 * to enum to support ErrorTypes.valueOf(ErrorTypes.IO.ordinal()) which would
 * return ErrorTypes.IO
 * 
 * @author ninti
 *
 */
public enum ErrorTypes {
	oK,
	connectionClosed, 
	malformedData,
	timoutReached,
	fileNotFound;
	
	private static Map<Integer, ErrorTypes> map = new HashMap<Integer, ErrorTypes>();

	static {
		for (ErrorTypes error : ErrorTypes.values()) {
			map.put(error.ordinal(), error);
		}
	}

	/**
	 * Returns the ErrorTypes of the ordinal position, <b>shall only be called
	 * with e.g ErrorTypes.IO.ordinal() as param
	 * 
	 * @param ordinalNr
	 *            the ordinal number of the ErrorType you want to get e.g
	 *            ErrorTypes.IO.ordinal()
	 * @return The ErrorType on the ordinal position
	 */
	public static ErrorTypes valueOf(int ordinalNr) {
		return map.get(ordinalNr);
	}

}
