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
package net.codecadenza.eclipse.model.db;

import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>DB Vendor Group Enumeration</b></em>', and utility methods for
 * working with them.
 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBVendorGroupEnumeration()
 * @model
 * @generated
 */
public enum DBVendorGroupEnumeration implements Enumerator {
	/**
	 * The '<em><b>MYSQL</b></em>' literal object
	 * @see #MYSQL_VALUE
	 * @generated
	 * @ordered
	 */
	MYSQL(0, "MYSQL", "MYSQL"),

	/**
	 * The '<em><b>ORACLE</b></em>' literal object
	 * @see #ORACLE_VALUE
	 * @generated
	 * @ordered
	 */
	ORACLE(1, "ORACLE", "ORACLE"),

	/**
	 * The '<em><b>H2</b></em>' literal object
	 * @see #H2_VALUE
	 * @generated
	 * @ordered
	 */
	H2(2, "H2", "H2"),

	/**
	 * The '<em><b>H2 EMBEDDED</b></em>' literal object
	 * @see #H2_EMBEDDED_VALUE
	 * @generated
	 * @ordered
	 */
	H2_EMBEDDED(3, "H2_EMBEDDED", "H2_EMBEDDED"),

	/**
	 * The '<em><b>POSTGRESQL</b></em>' literal object
	 * @see #POSTGRESQL_VALUE
	 * @generated
	 * @ordered
	 */
	POSTGRESQL(4, "POSTGRESQL", "POSTGRESQL"),

	/**
	 * The '<em><b>MSSQL</b></em>' literal object
	 * @see #MSSQL_VALUE
	 * @generated
	 * @ordered
	 */
	MSSQL(5, "MSSQL", "MSSQL");

	/**
	 * The '<em><b>MYSQL</b></em>' literal value
	 * @see #MYSQL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int MYSQL_VALUE = 0;

	/**
	 * The '<em><b>ORACLE</b></em>' literal value
	 * @see #ORACLE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int ORACLE_VALUE = 1;

	/**
	 * The '<em><b>H2</b></em>' literal value
	 * @see #H2
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int H2_VALUE = 2;

	/**
	 * The '<em><b>H2 EMBEDDED</b></em>' literal value
	 * @see #H2_EMBEDDED
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int H2_EMBEDDED_VALUE = 3;

	/**
	 * The '<em><b>POSTGRESQL</b></em>' literal value
	 * @see #POSTGRESQL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int POSTGRESQL_VALUE = 4;

	/**
	 * The '<em><b>MSSQL</b></em>' literal value
	 * @see #MSSQL
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int MSSQL_VALUE = 5;

	/**
	 * An array of all the '<em><b>DB Vendor Group Enumeration</b></em>' enumerators
	 * @generated
	 */
	private static final DBVendorGroupEnumeration[] VALUES_ARRAY = { MYSQL, ORACLE, H2, H2_EMBEDDED, POSTGRESQL, MSSQL };

	/**
	 * A public read-only list of all the '<em><b>DB Vendor Group Enumeration</b></em>' enumerators
	 * @generated
	 */
	public static final List<DBVendorGroupEnumeration> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>DB Vendor Group Enumeration</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static DBVendorGroupEnumeration get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>DB Vendor Group Enumeration</b></em>' literal with the specified name
	 * @generated
	 */
	public static DBVendorGroupEnumeration getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>DB Vendor Group Enumeration</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static DBVendorGroupEnumeration get(int value) {
		switch (value) {
			case MYSQL_VALUE:
				return MYSQL;
			case ORACLE_VALUE:
				return ORACLE;
			case H2_VALUE:
				return H2;
			case H2_EMBEDDED_VALUE:
				return H2_EMBEDDED;
			case POSTGRESQL_VALUE:
				return POSTGRESQL;
			case MSSQL_VALUE:
				return MSSQL;
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
	DBVendorGroupEnumeration(int value, String name, String literal) {
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
