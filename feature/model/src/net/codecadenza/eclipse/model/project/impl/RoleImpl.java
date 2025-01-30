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

import net.codecadenza.eclipse.model.project.ProjectPackage;
import net.codecadenza.eclipse.model.project.Role;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

/**
 * An implementation of the model object '<em><b>Role</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.RoleImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.RoleImpl#isAdminRole <em>Admin Role</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.project.impl.RoleImpl#isReadonlyRole <em>Readonly Role</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class RoleImpl extends EObjectImpl implements Role {
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
	 * The default value of the '{@link #isAdminRole() <em>Admin Role</em>}' attribute
	 * @see #isAdminRole()
	 * @generated
	 * @ordered
	 */
	protected static final boolean ADMIN_ROLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isAdminRole() <em>Admin Role</em>}' attribute
	 * @see #isAdminRole()
	 * @generated
	 * @ordered
	 */
	protected boolean adminRole = ADMIN_ROLE_EDEFAULT;

	/**
	 * The default value of the '{@link #isReadonlyRole() <em>Readonly Role</em>}' attribute
	 * @see #isReadonlyRole()
	 * @generated
	 * @ordered
	 */
	protected static final boolean READONLY_ROLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isReadonlyRole() <em>Readonly Role</em>}' attribute
	 * @see #isReadonlyRole()
	 * @generated
	 * @ordered
	 */
	protected boolean readonlyRole = READONLY_ROLE_EDEFAULT;

	/**
	 * @generated
	 */
	protected RoleImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ProjectPackage.Literals.ROLE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Role#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Role#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.ROLE__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Role#isAdminRole()
	 * @generated
	 */
	@Override
	public boolean isAdminRole() {
		return adminRole;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Role#setAdminRole(boolean)
	 * @generated
	 */
	@Override
	public void setAdminRole(boolean newAdminRole) {
		final boolean oldAdminRole = adminRole;
		adminRole = newAdminRole;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.ROLE__ADMIN_ROLE, oldAdminRole, adminRole));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Role#isReadonlyRole()
	 * @generated
	 */
	@Override
	public boolean isReadonlyRole() {
		return readonlyRole;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.project.Role#setReadonlyRole(boolean)
	 * @generated
	 */
	@Override
	public void setReadonlyRole(boolean newReadonlyRole) {
		final boolean oldReadonlyRole = readonlyRole;
		readonlyRole = newReadonlyRole;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ProjectPackage.ROLE__READONLY_ROLE, oldReadonlyRole, readonlyRole));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ProjectPackage.ROLE__NAME:
				return getName();
			case ProjectPackage.ROLE__ADMIN_ROLE:
				return isAdminRole();
			case ProjectPackage.ROLE__READONLY_ROLE:
				return isReadonlyRole();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case ProjectPackage.ROLE__NAME:
				setName((String) newValue);
				return;
			case ProjectPackage.ROLE__ADMIN_ROLE:
				setAdminRole((Boolean) newValue);
				return;
			case ProjectPackage.ROLE__READONLY_ROLE:
				setReadonlyRole((Boolean) newValue);
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
			case ProjectPackage.ROLE__NAME:
				setName(NAME_EDEFAULT);
				return;
			case ProjectPackage.ROLE__ADMIN_ROLE:
				setAdminRole(ADMIN_ROLE_EDEFAULT);
				return;
			case ProjectPackage.ROLE__READONLY_ROLE:
				setReadonlyRole(READONLY_ROLE_EDEFAULT);
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
			case ProjectPackage.ROLE__NAME:
				return name != null;
			case ProjectPackage.ROLE__ADMIN_ROLE:
				return adminRole != ADMIN_ROLE_EDEFAULT;
			case ProjectPackage.ROLE__READONLY_ROLE:
				return readonlyRole != READONLY_ROLE_EDEFAULT;
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
		result.append(", adminRole: ");
		result.append(adminRole);
		result.append(", readonlyRole: ");
		result.append(readonlyRole);
		result.append(')');

		return result.toString();
	}

}
