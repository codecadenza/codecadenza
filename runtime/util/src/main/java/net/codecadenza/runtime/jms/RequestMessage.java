/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.runtime.jms;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * <p>
 * Container object that holds all data of JMS request message
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RequestMessage implements Serializable {
	private static final long serialVersionUID = -7974013223415178981L;

	private final String operationID;
	private final List<Serializable> parameters = new ArrayList<>();
	private int parameterIndex;

	/**
	 * Constructor
	 * @param operationID the ID of the operation
	 */
	public RequestMessage(String operationID) {
		this.operationID = operationID;
	}

	/**
	 * @return the ID of the operation
	 */
	public String getOperationID() {
		return operationID;
	}

	/**
	 * Add a parameter to the request
	 * @param value the parameter value
	 * @return the actual object
	 */
	public RequestMessage withParameter(Serializable value) {
		parameters.add(value);
		return this;
	}

	/**
	 * Get the next parameter
	 * @return the value of the next parameter
	 * @throws IndexOutOfBoundsException if no further parameter exists
	 */
	public Serializable getNextParameter() {
		return parameters.get(parameterIndex++);
	}

	/**
	 * Get the next parameter
	 * @param type the expected type
	 * @return the value of the next parameter
	 * @throws IndexOutOfBoundsException if no further parameter exists
	 * @throws ClassCastException if the parameter has an unexpected type
	 */
	@SuppressWarnings({ "unchecked", "unused" })
	public <T> T getNextParameter(Class<T> type) {
		return (T) getNextParameter();
	}

	/**
	 * Get the next int parameter
	 * @return the value of the next int parameter
	 * @throws IndexOutOfBoundsException if no further parameter exists
	 * @throws ClassCastException if the parameter is no int
	 */
	public int getNextIntParameter() {
		return (int) getNextParameter();
	}

	/**
	 * Get the next {@link Integer} parameter
	 * @return the value of the next {@link Integer} parameter
	 * @throws IndexOutOfBoundsException if no further parameter exists
	 * @throws ClassCastException if the parameter is no {@link Integer}
	 */
	public Integer getNextIntegerParameter() {
		return getNextParameter(Integer.class);
	}

	/**
	 * Get the next long parameter
	 * @return the value of the next long parameter
	 * @throws IndexOutOfBoundsException if no further parameter exists
	 * @throws ClassCastException if the parameter is no long
	 */
	public long getNextLongParameter() {
		return (long) getNextParameter();
	}

	/**
	 * Get the next {@link Long} parameter
	 * @return the value of the next {@link Long} parameter
	 * @throws IndexOutOfBoundsException if no further parameter exists
	 * @throws ClassCastException if the parameter is no {@link Long}
	 */
	public Long getNextLongObjectParameter() {
		return getNextParameter(Long.class);
	}

	/**
	 * Get the next boolean parameter
	 * @return the value of the next boolean parameter
	 * @throws IndexOutOfBoundsException if no further parameter exists
	 * @throws ClassCastException if the parameter is no boolean
	 */
	public boolean getNextBoolParameter() {
		return (boolean) getNextParameter();
	}

	/**
	 * Get the next {@link Boolean} parameter
	 * @return the value of the next {@link Boolean} parameter
	 * @throws IndexOutOfBoundsException if no further parameter exists
	 * @throws ClassCastException if the parameter is no {@link Boolean}
	 */
	public Boolean getNextBooleanParameter() {
		return getNextParameter(Boolean.class);
	}

	/**
	 * Get the next float parameter
	 * @return the value of the next float parameter
	 * @throws IndexOutOfBoundsException if no further parameter exists
	 * @throws ClassCastException if the parameter is no float
	 */
	public float getNextFloatParameter() {
		return (float) getNextParameter();
	}

	/**
	 * Get the next {@link Float} parameter
	 * @return the value of the next {@link Float} parameter
	 * @throws IndexOutOfBoundsException if no further parameter exists
	 * @throws ClassCastException if the parameter is no {@link Float}
	 */
	public Float getNextFloatObjectParameter() {
		return getNextParameter(Float.class);
	}

	/**
	 * Get the next double parameter
	 * @return the value of the next double parameter
	 * @throws IndexOutOfBoundsException if no further parameter exists
	 * @throws ClassCastException if the parameter is no double
	 */
	public double getNextDoubleParameter() {
		return (double) getNextParameter();
	}

	/**
	 * Get the next {@link Double} parameter
	 * @return the value of the next {@link Double} parameter
	 * @throws IndexOutOfBoundsException if no further parameter exists
	 * @throws ClassCastException if the parameter is no {@link Double}
	 */
	public Double getNextDoubleObjectParameter() {
		return getNextParameter(Double.class);
	}

	/**
	 * Get the next {@link String} parameter
	 * @return the value of the next {@link String} parameter
	 * @throws IndexOutOfBoundsException if no further parameter exists
	 * @throws ClassCastException if the parameter is no {@link String}
	 */
	public String getNextStringParameter() {
		return getNextParameter(String.class);
	}

	/**
	 * Get the next {@link UUID} parameter
	 * @return the value of the next {@link UUID} parameter
	 * @throws IndexOutOfBoundsException if no further parameter exists
	 * @throws ClassCastException if the parameter is no {@link UUID}
	 */
	public UUID getNextUUIDParameter() {
		return getNextParameter(UUID.class);
	}

	/**
	 * Get the next char parameter
	 * @return the value of the next char parameter
	 * @throws IndexOutOfBoundsException if no further parameter exists
	 * @throws ClassCastException if the parameter is no char
	 */
	public char getNextCharParameter() {
		return (char) getNextParameter();
	}

}
