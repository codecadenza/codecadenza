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
package net.codecadenza.eclipse.tools.dbsync.model;

/**
 * <p>
 * Model for DDL transactions
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DDLTransaction {
	private int id;
	private String sql;
	private String errorMessage;
	private DDLTransactionState state;
	private DDLTransactionType type;
	private boolean execute = true;

	/**
	 * Constructor
	 * @param id the unique identifier of the transaction
	 * @param sql the DDL command
	 * @param errorMessage the error message
	 * @param state
	 * @param type
	 */
	public DDLTransaction(int id, String sql, String errorMessage, DDLTransactionState state, DDLTransactionType type) {
		this.id = id;
		this.sql = sql;
		this.errorMessage = errorMessage;
		this.state = state;
		this.type = type;
	}

	/**
	 * Constructor
	 * @param id the unique identifier of the transaction
	 * @param sql the DDL command
	 * @param type the transaction type
	 */
	public DDLTransaction(int id, String sql, DDLTransactionType type) {
		this.id = id;
		this.sql = sql;
		this.errorMessage = "";
		this.state = DDLTransactionState.OPEN;
		this.type = type;
	}

	/**
	 * @return the error message
	 */
	public String getErrorMessage() {
		return errorMessage;
	}

	/**
	 * Set the error message
	 * @param errorMessage
	 */
	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	/**
	 * @return the ID
	 */
	public int getId() {
		return id;
	}

	/**
	 * Set the ID of a transaction
	 * @param id
	 */
	public void setId(int id) {
		this.id = id;
	}

	/**
	 * @return the DDL script
	 */
	public String getSql() {
		return sql;
	}

	/**
	 * Set the DDL script
	 * @param sql
	 */
	public void setSql(String sql) {
		this.sql = sql;
	}

	/**
	 * @return the transaction state
	 */
	public DDLTransactionState getState() {
		return state;
	}

	/**
	 * Set the transaction state
	 * @param state
	 */
	public void setState(DDLTransactionState state) {
		this.state = state;
	}

	/**
	 * @return the transaction type
	 */
	public DDLTransactionType getType() {
		return type;
	}

	/**
	 * Set the transaction type
	 * @param type
	 */
	public void setType(DDLTransactionType type) {
		this.type = type;
	}

	/**
	 * @return true if the transaction should be executed
	 */
	public boolean isExecute() {
		return this.execute;
	}

	/**
	 * @param execute
	 */
	public void setExecute(boolean execute) {
		this.execute = execute;
	}

}
