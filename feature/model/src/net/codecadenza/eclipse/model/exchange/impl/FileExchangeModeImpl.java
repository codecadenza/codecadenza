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
package net.codecadenza.eclipse.model.exchange.impl;

import net.codecadenza.eclipse.model.exchange.ExchangePackage;
import net.codecadenza.eclipse.model.exchange.FileExchangeMode;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>File Exchange Mode</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.FileExchangeModeImpl#getPath <em>Path</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.FileExchangeModeImpl#getFileNamePattern <em>File Name Pattern</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.FileExchangeModeImpl#getBlockSize <em>Block Size</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.FileExchangeModeImpl#isNewTransactionPerFile <em>New Transaction Per
 * File</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.FileExchangeModeImpl#isDeleteAfterImport <em>Delete After
 * Import</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.FileExchangeModeImpl#getTargetPathAfterImport <em>Target Path After
 * Import</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class FileExchangeModeImpl extends DataExchangeModeImpl implements FileExchangeMode {
	/**
	 * The default value of the '{@link #getPath() <em>Path</em>}' attribute
	 * @see #getPath()
	 * @generated
	 * @ordered
	 */
	protected static final String PATH_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getPath() <em>Path</em>}' attribute
	 * @see #getPath()
	 * @generated
	 * @ordered
	 */
	protected String path = PATH_EDEFAULT;

	/**
	 * The default value of the '{@link #getFileNamePattern() <em>File Name Pattern</em>}' attribute
	 * @see #getFileNamePattern()
	 * @generated
	 * @ordered
	 */
	protected static final String FILE_NAME_PATTERN_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getFileNamePattern() <em>File Name Pattern</em>}' attribute
	 * @see #getFileNamePattern()
	 * @generated
	 * @ordered
	 */
	protected String fileNamePattern = FILE_NAME_PATTERN_EDEFAULT;

	/**
	 * The default value of the '{@link #getBlockSize() <em>Block Size</em>}' attribute
	 * @see #getBlockSize()
	 * @generated
	 * @ordered
	 */
	protected static final Integer BLOCK_SIZE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getBlockSize() <em>Block Size</em>}' attribute
	 * @see #getBlockSize()
	 * @generated
	 * @ordered
	 */
	protected Integer blockSize = BLOCK_SIZE_EDEFAULT;

	/**
	 * The default value of the '{@link #isNewTransactionPerFile() <em>New Transaction Per File</em>}' attribute
	 * @see #isNewTransactionPerFile()
	 * @generated
	 * @ordered
	 */
	protected static final boolean NEW_TRANSACTION_PER_FILE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isNewTransactionPerFile() <em>New Transaction Per File</em>}' attribute
	 * @see #isNewTransactionPerFile()
	 * @generated
	 * @ordered
	 */
	protected boolean newTransactionPerFile = NEW_TRANSACTION_PER_FILE_EDEFAULT;

	/**
	 * The default value of the '{@link #isDeleteAfterImport() <em>Delete After Import</em>}' attribute
	 * @see #isDeleteAfterImport()
	 * @generated
	 * @ordered
	 */
	protected static final boolean DELETE_AFTER_IMPORT_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isDeleteAfterImport() <em>Delete After Import</em>}' attribute
	 * @see #isDeleteAfterImport()
	 * @generated
	 * @ordered
	 */
	protected boolean deleteAfterImport = DELETE_AFTER_IMPORT_EDEFAULT;

	/**
	 * The default value of the '{@link #getTargetPathAfterImport() <em>Target Path After Import</em>}' attribute
	 * @see #getTargetPathAfterImport()
	 * @generated
	 * @ordered
	 */
	protected static final String TARGET_PATH_AFTER_IMPORT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getTargetPathAfterImport() <em>Target Path After Import</em>}' attribute
	 * @see #getTargetPathAfterImport()
	 * @generated
	 * @ordered
	 */
	protected String targetPathAfterImport = TARGET_PATH_AFTER_IMPORT_EDEFAULT;

	/**
	 * @generated
	 */
	protected FileExchangeModeImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.impl.DataExchangeModeImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ExchangePackage.Literals.FILE_EXCHANGE_MODE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#getPath()
	 * @generated
	 */
	@Override
	public String getPath() {
		return path;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#setPath(java.lang.String)
	 * @generated
	 */
	@Override
	public void setPath(String newPath) {
		final String oldPath = path;
		path = newPath;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.FILE_EXCHANGE_MODE__PATH, oldPath, path));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#getFileNamePattern()
	 * @generated
	 */
	@Override
	public String getFileNamePattern() {
		return fileNamePattern;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#setFileNamePattern(java.lang.String)
	 * @generated
	 */
	@Override
	public void setFileNamePattern(String newFileNamePattern) {
		final String oldFileNamePattern = fileNamePattern;
		fileNamePattern = newFileNamePattern;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.FILE_EXCHANGE_MODE__FILE_NAME_PATTERN,
					oldFileNamePattern, fileNamePattern));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#getBlockSize()
	 * @generated
	 */
	@Override
	public Integer getBlockSize() {
		return blockSize;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#setBlockSize(java.lang.Integer)
	 * @generated
	 */
	@Override
	public void setBlockSize(Integer newBlockSize) {
		final Integer oldBlockSize = blockSize;
		blockSize = newBlockSize;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ExchangePackage.FILE_EXCHANGE_MODE__BLOCK_SIZE, oldBlockSize, blockSize));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#isNewTransactionPerFile()
	 * @generated
	 */
	@Override
	public boolean isNewTransactionPerFile() {
		return newTransactionPerFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#setNewTransactionPerFile(boolean)
	 * @generated
	 */
	@Override
	public void setNewTransactionPerFile(boolean newNewTransactionPerFile) {
		final boolean oldNewTransactionPerFile = newTransactionPerFile;
		newTransactionPerFile = newNewTransactionPerFile;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.FILE_EXCHANGE_MODE__NEW_TRANSACTION_PER_FILE,
					oldNewTransactionPerFile, newTransactionPerFile));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#isDeleteAfterImport()
	 * @generated
	 */
	@Override
	public boolean isDeleteAfterImport() {
		return deleteAfterImport;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#setDeleteAfterImport(boolean)
	 * @generated
	 */
	@Override
	public void setDeleteAfterImport(boolean newDeleteAfterImport) {
		final boolean oldDeleteAfterImport = deleteAfterImport;
		deleteAfterImport = newDeleteAfterImport;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.FILE_EXCHANGE_MODE__DELETE_AFTER_IMPORT,
					oldDeleteAfterImport, deleteAfterImport));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#getTargetPathAfterImport()
	 * @generated
	 */
	@Override
	public String getTargetPathAfterImport() {
		return targetPathAfterImport;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#setTargetPathAfterImport(java.lang.String)
	 * @generated
	 */
	@Override
	public void setTargetPathAfterImport(String newTargetPathAfterImport) {
		final String oldTargetPathAfterImport = targetPathAfterImport;
		targetPathAfterImport = newTargetPathAfterImport;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.FILE_EXCHANGE_MODE__TARGET_PATH_AFTER_IMPORT,
					oldTargetPathAfterImport, targetPathAfterImport));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.impl.DataExchangeModeImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ExchangePackage.FILE_EXCHANGE_MODE__PATH:
				return getPath();
			case ExchangePackage.FILE_EXCHANGE_MODE__FILE_NAME_PATTERN:
				return getFileNamePattern();
			case ExchangePackage.FILE_EXCHANGE_MODE__BLOCK_SIZE:
				return getBlockSize();
			case ExchangePackage.FILE_EXCHANGE_MODE__NEW_TRANSACTION_PER_FILE:
				return isNewTransactionPerFile();
			case ExchangePackage.FILE_EXCHANGE_MODE__DELETE_AFTER_IMPORT:
				return isDeleteAfterImport();
			case ExchangePackage.FILE_EXCHANGE_MODE__TARGET_PATH_AFTER_IMPORT:
				return getTargetPathAfterImport();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.impl.DataExchangeModeImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case ExchangePackage.FILE_EXCHANGE_MODE__PATH:
				setPath((String) newValue);
				return;
			case ExchangePackage.FILE_EXCHANGE_MODE__FILE_NAME_PATTERN:
				setFileNamePattern((String) newValue);
				return;
			case ExchangePackage.FILE_EXCHANGE_MODE__BLOCK_SIZE:
				setBlockSize((Integer) newValue);
				return;
			case ExchangePackage.FILE_EXCHANGE_MODE__NEW_TRANSACTION_PER_FILE:
				setNewTransactionPerFile((Boolean) newValue);
				return;
			case ExchangePackage.FILE_EXCHANGE_MODE__DELETE_AFTER_IMPORT:
				setDeleteAfterImport((Boolean) newValue);
				return;
			case ExchangePackage.FILE_EXCHANGE_MODE__TARGET_PATH_AFTER_IMPORT:
				setTargetPathAfterImport((String) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.impl.DataExchangeModeImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case ExchangePackage.FILE_EXCHANGE_MODE__PATH:
				setPath(PATH_EDEFAULT);
				return;
			case ExchangePackage.FILE_EXCHANGE_MODE__FILE_NAME_PATTERN:
				setFileNamePattern(FILE_NAME_PATTERN_EDEFAULT);
				return;
			case ExchangePackage.FILE_EXCHANGE_MODE__BLOCK_SIZE:
				setBlockSize(BLOCK_SIZE_EDEFAULT);
				return;
			case ExchangePackage.FILE_EXCHANGE_MODE__NEW_TRANSACTION_PER_FILE:
				setNewTransactionPerFile(NEW_TRANSACTION_PER_FILE_EDEFAULT);
				return;
			case ExchangePackage.FILE_EXCHANGE_MODE__DELETE_AFTER_IMPORT:
				setDeleteAfterImport(DELETE_AFTER_IMPORT_EDEFAULT);
				return;
			case ExchangePackage.FILE_EXCHANGE_MODE__TARGET_PATH_AFTER_IMPORT:
				setTargetPathAfterImport(TARGET_PATH_AFTER_IMPORT_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.impl.DataExchangeModeImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case ExchangePackage.FILE_EXCHANGE_MODE__PATH:
				return path != null;
			case ExchangePackage.FILE_EXCHANGE_MODE__FILE_NAME_PATTERN:
				return fileNamePattern != null;
			case ExchangePackage.FILE_EXCHANGE_MODE__BLOCK_SIZE:
				return blockSize != null;
			case ExchangePackage.FILE_EXCHANGE_MODE__NEW_TRANSACTION_PER_FILE:
				return newTransactionPerFile != NEW_TRANSACTION_PER_FILE_EDEFAULT;
			case ExchangePackage.FILE_EXCHANGE_MODE__DELETE_AFTER_IMPORT:
				return deleteAfterImport != DELETE_AFTER_IMPORT_EDEFAULT;
			case ExchangePackage.FILE_EXCHANGE_MODE__TARGET_PATH_AFTER_IMPORT:
				return targetPathAfterImport != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.impl.DataExchangeModeImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (path: ");
		result.append(path);
		result.append(", fileNamePattern: ");
		result.append(fileNamePattern);
		result.append(", blockSize: ");
		result.append(blockSize);
		result.append(", newTransactionPerFile: ");
		result.append(newTransactionPerFile);
		result.append(", deleteAfterImport: ");
		result.append(deleteAfterImport);
		result.append(", targetPathAfterImport: ");
		result.append(targetPathAfterImport);
		result.append(')');

		return result.toString();
	}

}
