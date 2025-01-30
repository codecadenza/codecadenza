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

import org.eclipse.emf.ecore.EFactory;

/**
 * The factory for meta-model objects of package <b>DB</b>. It provides a create method for each non-abstract class of the model.
 * @see net.codecadenza.eclipse.model.db.DbPackage
 * @generated
 */
public interface DbFactory extends EFactory {
	/**
	 * The singleton instance of the factory
	 * @generated
	 */
	DbFactory eINSTANCE = net.codecadenza.eclipse.model.db.impl.DbFactoryImpl.init();

	/**
	 * @return a new {@link DBColumn} object
	 * @generated
	 */
	DBColumn createDBColumn();

	/**
	 * @return a new {@link DBColumnType} object
	 * @generated
	 */
	DBColumnType createDBColumnType();

	/**
	 * @return a new {@link DBIndex} object
	 * @generated
	 */
	DBIndex createDBIndex();

	/**
	 * @return a new {@link DBTable} object
	 * @generated
	 */
	DBTable createDBTable();

	/**
	 * @return a new {@link Database} object
	 * @generated
	 */
	Database createDatabase();

	/**
	 * @return a new object
	 * @generated
	 */
	ForeignKey createForeignKey();

	/**
	 * @return a new {@link PrimaryKey} object
	 * @generated
	 */
	PrimaryKey createPrimaryKey();

	/**
	 * @return the package supported by this factory
	 * @generated
	 */
	DbPackage getDbPackage();

}
