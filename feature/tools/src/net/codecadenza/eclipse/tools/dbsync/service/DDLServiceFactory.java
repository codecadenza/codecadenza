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
package net.codecadenza.eclipse.tools.dbsync.service;

import net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration;
import net.codecadenza.eclipse.tools.dbsync.service.imp.H2Implementation;
import net.codecadenza.eclipse.tools.dbsync.service.imp.MSSQLImplementation;
import net.codecadenza.eclipse.tools.dbsync.service.imp.MySQLImplementation;
import net.codecadenza.eclipse.tools.dbsync.service.imp.OracleImplementation;
import net.codecadenza.eclipse.tools.dbsync.service.imp.PostgreSQLImplementation;

/**
 * <p>
 * Factory class to get a DDL service depending on the database vendor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DDLServiceFactory {
	/**
	 * Prevent instantiation
	 */
	private DDLServiceFactory() {

	}

	/**
	 * Factory method to get a DDL service depending on the database vendor
	 * @param vendor the database vendor
	 * @return a DDL service bean
	 * @throws IllegalStateException if an implementation for the given database vendor is not available
	 */
	public static IDDLService getService(DBVendorGroupEnumeration vendor) {
		if (vendor == DBVendorGroupEnumeration.MYSQL)
			return new MySQLImplementation();
		else if (vendor == DBVendorGroupEnumeration.ORACLE)
			return new OracleImplementation();
		else if (vendor == DBVendorGroupEnumeration.H2 || vendor == DBVendorGroupEnumeration.H2_EMBEDDED)
			return new H2Implementation();
		else if (vendor == DBVendorGroupEnumeration.POSTGRESQL)
			return new PostgreSQLImplementation();
		else if (vendor == DBVendorGroupEnumeration.MSSQL)
			return new MSSQLImplementation();

		throw new IllegalStateException("A DDL service for the database vendor '" + vendor + "' is not available!");
	}

}
