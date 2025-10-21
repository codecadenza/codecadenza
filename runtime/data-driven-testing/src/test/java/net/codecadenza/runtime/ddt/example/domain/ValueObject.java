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
package net.codecadenza.runtime.ddt.example.domain;

/**
 * <p>
 * Class for testing simple Java types
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ValueObject {
	private int integerValue;
	private long longValue;
	private short shortValue;
	private byte byteValue;
	private char charValue;
	private boolean booleanValue;
	private double doubleValue;
	private float floatValue;
	private String stringValue;

	/**
	 * @return the integer value
	 */
	public int getIntegerValue() {
		return integerValue;
	}

	/**
	 * Set the integer value
	 * @param integerValue
	 */
	public void setIntegerValue(int integerValue) {
		this.integerValue = integerValue;
	}

	/**
	 * @return the long value
	 */
	public long getLongValue() {
		return longValue;
	}

	/**
	 * Set the long value
	 * @param longValue
	 */
	public void setLongValue(long longValue) {
		this.longValue = longValue;
	}

	/**
	 * @return the short value
	 */
	public short getShortValue() {
		return shortValue;
	}

	/**
	 * Set the short value
	 * @param shortValue
	 */
	public void setShortValue(short shortValue) {
		this.shortValue = shortValue;
	}

	/**
	 * @return the byte value
	 */
	public byte getByteValue() {
		return byteValue;
	}

	/**
	 * Set the byte value
	 * @param byteValue
	 */
	public void setByteValue(byte byteValue) {
		this.byteValue = byteValue;
	}

	/**
	 * @return the character value
	 */
	public char getCharValue() {
		return charValue;
	}

	/**
	 * Set the character value
	 * @param charValue
	 */
	public void setCharValue(char charValue) {
		this.charValue = charValue;
	}

	/**
	 * @return the boolean value
	 */
	public boolean isBooleanValue() {
		return booleanValue;
	}

	/**
	 * Set the boolean value
	 * @param booleanValue
	 */
	public void setBooleanValue(boolean booleanValue) {
		this.booleanValue = booleanValue;
	}

	/**
	 * @return the double value
	 */
	public double getDoubleValue() {
		return doubleValue;
	}

	/**
	 * Set the double value
	 * @param doubleValue
	 */
	public void setDoubleValue(double doubleValue) {
		this.doubleValue = doubleValue;
	}

	/**
	 * @return the float value
	 */
	public float getFloatValue() {
		return floatValue;
	}

	/**
	 * Set the float value
	 * @param floatValue
	 */
	public void setFloatValue(float floatValue) {
		this.floatValue = floatValue;
	}

	/**
	 * @return the string value
	 */
	public String getStringValue() {
		return stringValue;
	}

	/**
	 * Set the string value
	 * @param stringValue
	 */
	public void setStringValue(String stringValue) {
		this.stringValue = stringValue;
	}

}
