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
package net.codecadenza.eclipse.model.project;

import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>Integration Technology</b></em>', and utility methods for working
 * with them.
 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getIntegrationTechnology()
 * @model
 * @generated
 */
public enum IntegrationTechnology implements Enumerator {
	/**
	 * The '<em><b>REST</b></em>' literal object
	 * @see #REST_VALUE
	 * @generated
	 * @ordered
	 */
	REST(0, "REST", "REST"),

	/**
	 * The '<em><b>SOAP</b></em>' literal object
	 * @see #SOAP_VALUE
	 * @generated
	 * @ordered
	 */
	SOAP(1, "SOAP", "SOAP"),

	/**
	 * The '<em><b>RMI</b></em>' literal object
	 * @see #RMI_VALUE
	 * @generated
	 * @ordered
	 */
	RMI(2, "RMI", "RMI"),

	/**
	 * The '<em><b>KAFKA</b></em>' literal object
	 * @see #KAFKA_VALUE
	 * @generated
	 * @ordered
	 */
	KAFKA(3, "KAFKA", "KAFKA"),

	/**
	 * The '<em><b>JMS</b></em>' literal object
	 * @see #JMS_VALUE
	 * @generated
	 * @ordered
	 */
	JMS(4, "JMS", "JMS");

	/**
	 * The '<em><b>REST</b></em>' literal value
	 * @see #REST
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int REST_VALUE = 0;

	/**
	 * The '<em><b>SOAP</b></em>' literal value
	 * @see #SOAP
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SOAP_VALUE = 1;

	/**
	 * The '<em><b>RMI</b></em>' literal value
	 * @see #RMI
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int RMI_VALUE = 2;

	/**
	 * The '<em><b>KAFKA</b></em>' literal value
	 * @see #KAFKA
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int KAFKA_VALUE = 3;

	/**
	 * The '<em><b>JMS</b></em>' literal value
	 * @see #JMS
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int JMS_VALUE = 4;

	/**
	 * An array of all the '<em><b>Integration Technology</b></em>' enumerators
	 * @generated
	 */
	private static final IntegrationTechnology[] VALUES_ARRAY = { REST, SOAP, RMI, KAFKA, JMS };

	/**
	 * A public read-only list of all the '<em><b>Integration Technology</b></em>' enumerators
	 * @generated
	 */
	public static final List<IntegrationTechnology> VALUES = List.of(VALUES_ARRAY);

	/**
	 * Return the '<em><b>Integration Technology</b></em>' literal with the specified literal value
	 * @param literal the literal
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static IntegrationTechnology get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * Return the '<em><b>Integration Technology</b></em>' literal with the specified name
	 * @param name the name
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static IntegrationTechnology getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * Return the '<em><b>Integration Technology</b></em>' literal with the specified integer value
	 * @param value the integer value
	 * @return the matching enumerator or null
	 * @generated
	 */
	public static IntegrationTechnology get(int value) {
		switch (value) {
			case REST_VALUE:
				return REST;
			case SOAP_VALUE:
				return SOAP;
			case RMI_VALUE:
				return RMI;
			case KAFKA_VALUE:
				return KAFKA;
			case JMS_VALUE:
				return JMS;
		}
		return null;
	}

	/**
	 * @generated
	 */
	private final int value;

	/**
	 * @generated
	 */
	private final String name;

	/**
	 * @generated
	 */
	private final String literal;

	/**
	 * Constructor
	 * @param value
	 * @param name
	 * @param literal
	 * @generated
	 */
	IntegrationTechnology(int value, String name, String literal) {
		this.value = value;
		this.name = name;
		this.literal = literal;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.util.Enumerator#getValue()
	 * @generated
	 */
	@Override
	public int getValue() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.util.Enumerator#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.util.Enumerator#getLiteral()
	 * @generated
	 */
	@Override
	public String getLiteral() {
		return literal;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Enum#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		return literal;
	}

}
