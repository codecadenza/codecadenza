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
package net.codecadenza.eclipse.tools.dbsync.service.imp;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashSet;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBNamingUtil;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.ForeignKey;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.tools.dbsync.service.IDDLService;
import net.codecadenza.eclipse.tools.util.db.DBManager;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * H2 implementation for creating DDL scripts
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class H2Implementation implements IDDLService {
	/**
	 * @param col
	 * @return the generated fragment of a column statement
	 */
	private String generateColumnStatement(DBColumn col) {
		final var b = new StringBuilder();
		b.append(col.getDatabaseName() + " " + col.getColumnType().getName());

		if (col.getLength() > 0) {
			b.append("(" + col.getLength());
			b.append(")");
		}

		if (col.getPrecision() > 0) {
			b.append("(" + col.getPrecision());

			if (col.getScale() > 0)
				b.append("," + col.getScale());

			b.append(")");
		}

		if (!col.isNullable())
			b.append(" not null");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.dbsync.service.IDDLService#createSequence(java.lang.String, int, int)
	 */
	@Override
	public String createSequence(String name, int startIndex, int blockSize) {
		final var b = new StringBuilder();
		b.append("create sequence ");
		b.append(name + " minvalue " + startIndex + " ");
		b.append("increment by ");
		b.append(blockSize);

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.dbsync.service.IDDLService#createPrimaryKey(net.codecadenza.eclipse.model.db.DBTable)
	 */
	@Override
	public String createPrimaryKey(DBTable table) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.dbsync.service.IDDLService#dropPrimaryKey(net.codecadenza.eclipse.model.db.DBTable)
	 */
	@Override
	public String dropPrimaryKey(DBTable table) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.dbsync.service.IDDLService#addColumn(net.codecadenza.eclipse.model.db.DBColumn)
	 */
	@Override
	public String addColumn(DBColumn col) {
		final var b = new StringBuilder();
		b.append("alter table " + col.getDatabaseTable().getFullDatabaseName());
		b.append(" add column ");
		b.append(generateColumnStatement(col));

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.dbsync.service.IDDLService#addForeignKey(net.codecadenza.eclipse.model.db.ForeignKey)
	 */
	@Override
	public String addForeignKey(ForeignKey key) {
		final var b = new StringBuilder();
		b.append("alter table " + key.getTable().getFullDatabaseName());
		b.append(" add constraint " + key.getDatabaseName() + " foreign key (");
		b.append(key.getColumn().getDatabaseName() + ") references ");
		b.append(key.getReferencedColumn().getDatabaseTable().getFullDatabaseName() + "(");
		b.append(key.getReferencedColumn().getDatabaseName() + ") on delete cascade on update restrict");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.dbsync.service.IDDLService#addUniqueKey(net.codecadenza.eclipse.model.db.DBIndex)
	 */
	@Override
	public String addUniqueKey(DBIndex index) {
		var b = new StringBuilder();
		b.append("create unique index " + index.getDatabaseName() + " on " + index.getTable().getFullDatabaseName() + "(");

		for (final DBColumn col : index.getColumns())
			b.append(col.getDatabaseName() + ", ");

		b = new StringBuilder(b.substring(0, b.length() - 2));
		b.append(")");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.dbsync.service.IDDLService#createIndex(net.codecadenza.eclipse.model.db.DBIndex)
	 */
	@Override
	public String createIndex(DBIndex index) {
		var b = new StringBuilder();
		b.append("create index " + index.getDatabaseName() + " on " + index.getTable().getFullDatabaseName() + "(");

		for (final DBColumn col : index.getColumns())
			b.append(col.getDatabaseName() + ", ");

		b = new StringBuilder(b.substring(0, b.length() - 2));
		b.append(")");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.dbsync.service.IDDLService#removeColumn(net.codecadenza.eclipse.model.db.DBColumn)
	 */
	@Override
	public String removeColumn(DBColumn column) {
		final var b = new StringBuilder();
		b.append("alter table " + column.getDatabaseTable().getFullDatabaseName());
		b.append(" drop column " + column.getDatabaseName());

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.dbsync.service.IDDLService#removeForeignKey(net.codecadenza.eclipse.model.db.ForeignKey)
	 */
	@Override
	public String removeForeignKey(ForeignKey key) {
		final var b = new StringBuilder();
		b.append("alter table " + key.getTable().getFullDatabaseName());
		b.append(" drop foreign key " + key.getDatabaseName());

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.dbsync.service.IDDLService#modifyColumn(net.codecadenza.eclipse.model.db.DBColumn)
	 */
	@Override
	public String modifyColumn(DBColumn column) {
		final var b = new StringBuilder();
		b.append("alter table " + column.getDatabaseTable().getFullDatabaseName());
		b.append(" alter column " + column.getDatabaseName() + " ");

		// H2 doesn't support a column modification command in order to change all parameters in one statement! Thus, we can just
		// change the type and the length or the nullable constraint!
		if (column.getLength() > 0)
			b.append("set data type " + column.getColumnType().getName() + "(" + column.getLength() + ")");
		else {
			if (!column.isNullable())
				b.append("not ");

			b.append("null");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.dbsync.service.IDDLService#removeIndex(net.codecadenza.eclipse.model.db.DBIndex)
	 */
	@Override
	public String removeIndex(DBIndex index) {
		final var b = new StringBuilder();
		b.append("drop index ");
		b.append(index.getFullDatabaseName());

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.dbsync.service.IDDLService#removeUniqueKey(net.codecadenza.eclipse.model.db.DBIndex)
	 */
	@Override
	public String removeUniqueKey(DBIndex index) {
		return removeIndex(index);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.dbsync.service.IDDLService#createTable(net.codecadenza.eclipse.model.db.DBTable)
	 */
	@Override
	public String createTable(DBTable table) {
		final EList<DomainObject> domainObjects = table.getDatabase().getProject().getAllDomainObjectsOfProject(false, true);
		final DomainObject domainObject = domainObjects.stream().filter(d -> table.equals(d.getDatabaseTable())).findFirst()
				.orElse(null);
		boolean hasAutoIncrement = false;
		var b = new StringBuilder();

		b.append("create table " + table.getFullDatabaseName());

		if (!table.getColumns().isEmpty())
			b.append("(");

		// Check if the table uses an auto-increment value
		if (domainObject != null && domainObject.getIDGenerator().getGeneratorType() == IDGeneratorTypeEnumeration.IDENTITY)
			hasAutoIncrement = true;

		for (final DBColumn col : table.getColumns()) {
			boolean addAutoincrement = false;

			boolean addPrimaryKey = false;

			// Check if this column should be supplied with a primary key
			if (table.getPrimaryKey() != null && table.getPrimaryKey().getColumn().equals(col)) {
				if (hasAutoIncrement)
					addAutoincrement = true;

				addPrimaryKey = true;
			}

			// Check if this column should be supplied with an auto-increment primary key
			if (table.getPrimaryKey() != null && table.getPrimaryKey().getColumn().equals(col) && hasAutoIncrement)
				addAutoincrement = true;

			b.append(generateColumnStatement(col));

			if (addAutoincrement)
				b.append(" generated always as identity");

			if (addPrimaryKey)
				b.append(" primary key");

			b.append(", ");
		}

		b = new StringBuilder(b.substring(0, b.length() - 2));
		b.append(")");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.dbsync.service.IDDLService#dropTable(net.codecadenza.eclipse.model.db.DBTable)
	 */
	@Override
	public String dropTable(DBTable table) {
		final var b = new StringBuilder();
		b.append("drop table ");
		b.append(table.getFullDatabaseName());

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.dbsync.service.IDDLService#renameColumn(java.lang.String,
	 * net.codecadenza.eclipse.model.db.DBColumn)
	 */
	@Override
	public String renameColumn(String newName, DBColumn column) {
		final var b = new StringBuilder();
		b.append("rename column " + column.getDatabaseTable().getFullDatabaseName());
		b.append("." + column.getDatabaseName() + " to " + newName);

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.dbsync.service.IDDLService#renameTable(java.lang.String,
	 * net.codecadenza.eclipse.model.db.DBTable)
	 */
	@Override
	public String renameTable(String oldName, DBTable table) {
		final var b = new StringBuilder();
		b.append("alter table ");
		b.append(oldName);
		b.append(" rename to ");
		b.append(table.getDatabaseName());

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.dbsync.service.IDDLService#getAllSequences(net.codecadenza.eclipse.model.db.Database,
	 * net.codecadenza.eclipse.tools.util.db.DBManager, java.lang.String, java.lang.String)
	 */
	@Override
	public HashSet<String> getAllSequences(Database database, DBManager dbManager, String catalogName, String schemaName) {
		final var sequences = new HashSet<String>();
		final var query = "select sequence_name from information_schema.sequences";

		try (Connection con = dbManager.getConnection();
				Statement st = con.createStatement();
				ResultSet rs = st.executeQuery(query)) {
			while (rs.next()) {
				final String sequence = DBNamingUtil.convertToStyle(rs.getString("sequence_name"), database);
				sequences.add(sequence);
			}

			return sequences;
		}
		catch (final Exception e) {
			throw new RuntimeException(e);
		}
	}

}
