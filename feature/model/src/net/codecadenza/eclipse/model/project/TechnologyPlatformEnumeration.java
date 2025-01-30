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
 * A representation of the literals of the enumeration '<em><b>Technology Platform Enumeration</b></em>', and utility methods for
 * working with them.
 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getTechnologyPlatformEnumeration()
 * @model
 * @generated
 */
public enum TechnologyPlatformEnumeration implements Enumerator {
	/**
	 * The '<em><b>JAKARTA_EE</b></em>' literal object
	 * @see #JAKARTA_EE_VALUE
	 * @generated
	 * @ordered
	 */
	JAKARTA_EE(1, "JAKARTA_EE", "JAKARTA_EE"),

	/**
	 * The '<em><b>JAVA_SE</b></em>' literal object
	 * @see #JAVA_SE_VALUE
	 * @generated
	 * @ordered
	 */
	JAVA_SE(2, "JAVA_SE", "JAVA_SE"),

	/**
	 * The '<em><b>SPRING_BOOT</b></em>' literal object
	 * @see #SPRING_BOOT_VALUE
	 * @generated
	 * @ordered
	 */
	SPRING_BOOT(3, "SPRING_BOOT", "SPRING_BOOT");

	/**
	 * The '<em><b>JAKARTA_EE</b></em>' literal value
	 * @see #JAKARTA_EE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int JAKARTA_EE_VALUE = 1;

	/**
	 * The '<em><b>JAVA_SE</b></em>' literal value
	 * @see #JAVA_SE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int JAVA_SE_VALUE = 2;

	/**
	 * The '<em><b>SPRING_BOOT</b></em>' literal value
	 * @see #SPRING_BOOT
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SPRING_BOOT_VALUE = 3;

	/**
	 * An array of all the '<em><b>Technology Platform Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final TechnologyPlatformEnumeration[] VALUES_ARRAY = { JAKARTA_EE, JAVA_SE, SPRING_BOOT };

	/**
	 * A public read-only list of all the '<em><b>Technology Platform Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<TechnologyPlatformEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Technology Platform Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static TechnologyPlatformEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Technology Platform Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static TechnologyPlatformEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Technology Platform Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static TechnologyPlatformEnumeration get(int value) {
		switch (value) {
			case JAKARTA_EE_VALUE:
				return JAKARTA_EE;
			case JAVA_SE_VALUE:
				return JAVA_SE;
			case SPRING_BOOT_VALUE:
				return SPRING_BOOT;
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
	TechnologyPlatformEnumeration(int value, String name, String literal) {
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
