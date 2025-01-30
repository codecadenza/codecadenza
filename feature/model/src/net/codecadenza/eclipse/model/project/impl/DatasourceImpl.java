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
package net.codecadenza.eclipse.model.project.impl;

import java.util.Collection;
import net.codecadenza.eclipse.model.project.Datasource;
import net.codecadenza.eclipse.model.project.ProjectPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EDataTypeUniqueEList;

/**
 * An implementation of the model object '<em><b>Datasource</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.DatasourceImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.DatasourceImpl#getConnectionURL <em>Connection URL</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.DatasourceImpl#getUserName <em>User Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.DatasourceImpl#getPassword <em>Password</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.DatasourceImpl#getDriverName <em>Driver Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.DatasourceImpl#getDriverList <em>Driver List</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class DatasourceImpl extends EObjectImpl implements Datasource {
	/**
	 * The default value of the '{@link #getName() <em>Name</em>}' attribute
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected static final String NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getName() <em>Name</em>}' attribute
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected String name = NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #getConnectionURL() <em>Connection URL</em>}' attribute
	 * @see #getConnectionURL()
	 * @generated
	 * @ordered
	 */
	protected static final String CONNECTION_URL_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getConnectionURL() <em>Connection URL</em>}' attribute
	 * @see #getConnectionURL()
	 * @generated
	 * @ordered
	 */
	protected String connectionURL = CONNECTION_URL_EDEFAULT;

	/**
	 * The default value of the '{@link #getUserName() <em>User Name</em>}' attribute
	 * @see #getUserName()
	 * @generated
	 * @ordered
	 */
	protected static final String USER_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getUserName() <em>User Name</em>}' attribute
	 * @see #getUserName()
	 * @generated
	 * @ordered
	 */
	protected String userName = USER_NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #getPassword() <em>Password</em>}' attribute
	 * @see #getPassword()
	 * @generated
	 * @ordered
	 */
	protected static final String PASSWORD_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getPassword() <em>Password</em>}' attribute
	 * @see #getPassword()
	 * @generated
	 * @ordered
	 */
	protected String password = PASSWORD_EDEFAULT;

	/**
	 * The default value of the '{@link #getDriverName() <em>Driver Name</em>}' attribute
	 * @see #getDriverName()
	 * @generated
	 * @ordered
	 */
	protected static final String DRIVER_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDriverName() <em>Driver Name</em>}' attribute
	 * @see #getDriverName()
	 * @generated
	 * @ordered
	 */
	protected String driverName = DRIVER_NAME_EDEFAULT;

	/**
	 * The cached value of the '{@link #getDriverList() <em>Driver List</em>}' attribute list
	 * @see #getDriverList()
	 * @generated
	 * @ordered
	 */
	protected EList<String> driverList;

	/**
	 * @generated
	 */
	protected DatasourceImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ProjectPackage.Literals.DATASOURCE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Datasource#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Datasource#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.DATASOURCE__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Datasource#getConnectionURL()
	 * @generated
	 */
	@Override
	public String getConnectionURL() {
		return connectionURL;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Datasource#setConnectionURL(java.lang.String)
	 * @generated
	 */
	@Override
	public void setConnectionURL(String newConnectionURL) {
		final String oldConnectionURL = connectionURL;
		connectionURL = newConnectionURL;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.DATASOURCE__CONNECTION_URL, oldConnectionURL,
					connectionURL));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Datasource#getUserName()
	 * @generated
	 */
	@Override
	public String getUserName() {
		return userName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Datasource#setUserName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setUserName(String newUserName) {
		final String oldUserName = userName;
		userName = newUserName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.DATASOURCE__USER_NAME, oldUserName, userName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Datasource#getPassword()
	 * @generated
	 */
	@Override
	public String getPassword() {
		return password;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Datasource#setPassword(java.lang.String)
	 * @generated
	 */
	@Override
	public void setPassword(String newPassword) {
		final String oldPassword = password;
		password = newPassword;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.DATASOURCE__PASSWORD, oldPassword, password));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Datasource#getDriverName()
	 * @generated
	 */
	@Override
	public String getDriverName() {
		return driverName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Datasource#setDriverName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setDriverName(String newDriverName) {
		final String oldDriverName = driverName;
		driverName = newDriverName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.DATASOURCE__DRIVER_NAME, oldDriverName, driverName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Datasource#getDriverList()
	 * @generated
	 */
	@Override
	public EList<String> getDriverList() {
		if (driverList == null)
			driverList = new EDataTypeUniqueEList<>(String.class, this, ProjectPackage.DATASOURCE__DRIVER_LIST);

		return driverList;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ProjectPackage.DATASOURCE__NAME:
				return getName();
			case ProjectPackage.DATASOURCE__CONNECTION_URL:
				return getConnectionURL();
			case ProjectPackage.DATASOURCE__USER_NAME:
				return getUserName();
			case ProjectPackage.DATASOURCE__PASSWORD:
				return getPassword();
			case ProjectPackage.DATASOURCE__DRIVER_NAME:
				return getDriverName();
			case ProjectPackage.DATASOURCE__DRIVER_LIST:
				return getDriverList();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case ProjectPackage.DATASOURCE__NAME:
				setName((String) newValue);
				return;
			case ProjectPackage.DATASOURCE__CONNECTION_URL:
				setConnectionURL((String) newValue);
				return;
			case ProjectPackage.DATASOURCE__USER_NAME:
				setUserName((String) newValue);
				return;
			case ProjectPackage.DATASOURCE__PASSWORD:
				setPassword((String) newValue);
				return;
			case ProjectPackage.DATASOURCE__DRIVER_NAME:
				setDriverName((String) newValue);
				return;
			case ProjectPackage.DATASOURCE__DRIVER_LIST:
				getDriverList().clear();
				getDriverList().addAll((Collection<String>) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case ProjectPackage.DATASOURCE__NAME:
				setName(NAME_EDEFAULT);
				return;
			case ProjectPackage.DATASOURCE__CONNECTION_URL:
				setConnectionURL(CONNECTION_URL_EDEFAULT);
				return;
			case ProjectPackage.DATASOURCE__USER_NAME:
				setUserName(USER_NAME_EDEFAULT);
				return;
			case ProjectPackage.DATASOURCE__PASSWORD:
				setPassword(PASSWORD_EDEFAULT);
				return;
			case ProjectPackage.DATASOURCE__DRIVER_NAME:
				setDriverName(DRIVER_NAME_EDEFAULT);
				return;
			case ProjectPackage.DATASOURCE__DRIVER_LIST:
				getDriverList().clear();
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case ProjectPackage.DATASOURCE__NAME:
				return name != null;
			case ProjectPackage.DATASOURCE__CONNECTION_URL:
				return connectionURL != null;
			case ProjectPackage.DATASOURCE__USER_NAME:
				return userName != null;
			case ProjectPackage.DATASOURCE__PASSWORD:
				return password != null;
			case ProjectPackage.DATASOURCE__DRIVER_NAME:
				return driverName != null;
			case ProjectPackage.DATASOURCE__DRIVER_LIST:
				return driverList != null && !driverList.isEmpty();
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (name: ");
		result.append(name);
		result.append(", connectionURL: ");
		result.append(connectionURL);
		result.append(", userName: ");
		result.append(userName);
		result.append(", password: ");
		result.append(password);
		result.append(", driverName: ");
		result.append(driverName);
		result.append(", driverList: ");
		result.append(driverList);
		result.append(')');

		return result.toString();
	}

}
