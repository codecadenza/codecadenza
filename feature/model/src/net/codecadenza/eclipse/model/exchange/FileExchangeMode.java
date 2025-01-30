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
package net.codecadenza.eclipse.model.exchange;

/**
 * A representation of the model object '<em><b>File Exchange Mode</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#getPath <em>Path</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#getFileNamePattern <em>File Name Pattern</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#getBlockSize <em>Block Size</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#isNewTransactionPerFile <em>New Transaction Per
 * File</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#isDeleteAfterImport <em>Delete After Import</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#getTargetPathAfterImport <em>Target Path After
 * Import</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFileExchangeMode()
 * @model
 * @generated
 */
public interface FileExchangeMode extends DataExchangeMode {
	/**
	 * Return the value of the '<em><b>Path</b></em>' attribute
	 * @return the value of the '<em>Path</em>' attribute
	 * @see #setPath(String)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFileExchangeMode_Path()
	 * @model
	 * @generated
	 */
	String getPath();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#getPath <em>Path</em>}' attribute
	 * @param value the new value of the '<em>Path</em>' attribute
	 * @see #getPath()
	 * @generated
	 */
	void setPath(String value);

	/**
	 * Return the value of the '<em><b>File Name Pattern</b></em>' attribute
	 * @return the value of the '<em>File Name Pattern</em>' attribute
	 * @see #setFileNamePattern(String)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFileExchangeMode_FileNamePattern()
	 * @model
	 * @generated
	 */
	String getFileNamePattern();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#getFileNamePattern <em>File Name
	 * Pattern</em>}' attribute
	 * @param value the new value of the '<em>File Name Pattern</em>' attribute
	 * @see #getFileNamePattern()
	 * @generated
	 */
	void setFileNamePattern(String value);

	/**
	 * Return the value of the '<em><b>Block Size</b></em>' attribute
	 * @return the value of the '<em>Block Size</em>' attribute
	 * @see #setBlockSize(Integer)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFileExchangeMode_BlockSize()
	 * @model
	 * @generated
	 */
	Integer getBlockSize();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#getBlockSize <em>Block Size</em>}'
	 * attribute
	 * @param value the new value of the '<em>Block Size</em>' attribute
	 * @see #getBlockSize()
	 * @generated
	 */
	void setBlockSize(Integer value);

	/**
	 * Return the value of the '<em><b>New Transaction Per File</b></em>' attribute
	 * @return the value of the '<em>New Transaction Per File</em>' attribute
	 * @see #setNewTransactionPerFile(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFileExchangeMode_NewTransactionPerFile()
	 * @model
	 * @generated
	 */
	boolean isNewTransactionPerFile();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#isNewTransactionPerFile <em>New
	 * Transaction Per File</em>}' attribute
	 * @param value the new value of the '<em>New Transaction Per File</em>' attribute
	 * @see #isNewTransactionPerFile()
	 * @generated
	 */
	void setNewTransactionPerFile(boolean value);

	/**
	 * Return the value of the '<em><b>Delete After Import</b></em>' attribute
	 * @return the value of the '<em>Delete After Import</em>' attribute
	 * @see #setDeleteAfterImport(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFileExchangeMode_DeleteAfterImport()
	 * @model
	 * @generated
	 */
	boolean isDeleteAfterImport();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#isDeleteAfterImport <em>Delete After
	 * Import</em>}' attribute
	 * @param value the new value of the '<em>Delete After Import</em>' attribute
	 * @see #isDeleteAfterImport()
	 * @generated
	 */
	void setDeleteAfterImport(boolean value);

	/**
	 * Return the value of the '<em><b>Target Path After Import</b></em>' attribute
	 * @return the value of the '<em>Target Path After Import</em>' attribute
	 * @see #setTargetPathAfterImport(String)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFileExchangeMode_TargetPathAfterImport()
	 * @model
	 * @generated
	 */
	String getTargetPathAfterImport();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#getTargetPathAfterImport <em>Target Path
	 * After Import</em>}' attribute
	 * @param value the new value of the '<em>Target Path After Import</em>' attribute
	 * @see #getTargetPathAfterImport()
	 * @generated
	 */
	void setTargetPathAfterImport(String value);

}
