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
package net.codecadenza.eclipse.model.db.impl;

import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbFactory;
import net.codecadenza.eclipse.model.db.DbPackage;
import net.codecadenza.eclipse.model.db.ForeignKey;
import net.codecadenza.eclipse.model.db.IdentifierStyleEnumeration;
import net.codecadenza.eclipse.model.db.PrimaryKey;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EFactoryImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * An implementation of the model factory.
 * @generated
 */
public class DbFactoryImpl extends EFactoryImpl implements DbFactory {
	/**
	 * @return the default factory implementation
	 * @generated
	 */
	public static DbFactory init() {
		try {
			final var theDbFactory = (DbFactory) EPackage.Registry.INSTANCE.getEFactory(DbPackage.eNS_URI);

			if (theDbFactory != null)
				return theDbFactory;
		}
		catch (final Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}

		return new DbFactoryImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#create(org.eclipse.emf.ecore.EClass)
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		return switch (eClass.getClassifierID()) {
			case DbPackage.DB_COLUMN -> createDBColumn();
			case DbPackage.DB_COLUMN_TYPE -> createDBColumnType();
			case DbPackage.DB_INDEX -> createDBIndex();
			case DbPackage.DB_TABLE -> createDBTable();
			case DbPackage.DATABASE -> createDatabase();
			case DbPackage.FOREIGN_KEY -> createForeignKey();
			case DbPackage.PRIMARY_KEY -> createPrimaryKey();
			default -> throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#createFromString(org.eclipse.emf.ecore.EDataType, java.lang.String)
	 * @generated
	 */
	@Override
	public Object createFromString(EDataType eDataType, String initialValue) {
		return switch (eDataType.getClassifierID()) {
			case DbPackage.DB_VENDOR_GROUP_ENUMERATION -> createDBVendorGroupEnumerationFromString(eDataType, initialValue);
			case DbPackage.IDENTIFIER_STYLE_ENUMERATION -> createIdentifierStyleEnumerationFromString(eDataType, initialValue);
			default -> throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#convertToString(org.eclipse.emf.ecore.EDataType, java.lang.Object)
	 * @generated
	 */
	@Override
	public String convertToString(EDataType eDataType, Object instanceValue) {
		return switch (eDataType.getClassifierID()) {
			case DbPackage.DB_VENDOR_GROUP_ENUMERATION -> convertDBVendorGroupEnumerationToString(eDataType, instanceValue);
			case DbPackage.IDENTIFIER_STYLE_ENUMERATION -> convertIdentifierStyleEnumerationToString(eDataType, instanceValue);
			default -> throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbFactory#createDBColumn()
	 * @generated
	 */
	@Override
	public DBColumn createDBColumn() {
		return new DBColumnImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbFactory#createDBColumnType()
	 * @generated
	 */
	@Override
	public DBColumnType createDBColumnType() {
		return new DBColumnTypeImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbFactory#createDBIndex()
	 * @generated
	 */
	@Override
	public DBIndex createDBIndex() {
		return new DBIndexImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbFactory#createDBTable()
	 * @generated
	 */
	@Override
	public DBTable createDBTable() {
		return new DBTableImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbFactory#createDatabase()
	 * @generated
	 */
	@Override
	public Database createDatabase() {
		return new DatabaseImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbFactory#createForeignKey()
	 * @generated
	 */
	@Override
	public ForeignKey createForeignKey() {
		return new ForeignKeyImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbFactory#createPrimaryKey()
	 * @generated
	 */
	@Override
	public PrimaryKey createPrimaryKey() {
		return new PrimaryKeyImpl();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public DBVendorGroupEnumeration createDBVendorGroupEnumerationFromString(EDataType eDataType, String initialValue) {
		final DBVendorGroupEnumeration result = DBVendorGroupEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertDBVendorGroupEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public IdentifierStyleEnumeration createIdentifierStyleEnumerationFromString(EDataType eDataType, String initialValue) {
		final IdentifierStyleEnumeration result = IdentifierStyleEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertIdentifierStyleEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbFactory#getDbPackage()
	 * @generated
	 */
	@Override
	public DbPackage getDbPackage() {
		return (DbPackage) getEPackage();
	}

	/**
	 * @deprecated
	 * @return the database package
	 * @generated
	 */
	@Deprecated
	public static DbPackage getPackage() {
		return DbPackage.eINSTANCE;
	}

}
