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
package net.codecadenza.eclipse.model.domain.impl;

import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>One To One Association</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.OneToOneAssociationImpl#isOptional <em>Optional</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.OneToOneAssociationImpl#getColumn <em>Column</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class OneToOneAssociationImpl extends AbstractDomainAssociationImpl implements OneToOneAssociation {
	/**
	 * The default value of the '{@link #isOptional() <em>Optional</em>}' attribute
	 * @see #isOptional()
	 * @generated
	 * @ordered
	 */
	protected static final boolean OPTIONAL_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isOptional() <em>Optional</em>}' attribute
	 * @see #isOptional()
	 * @generated
	 * @ordered
	 */
	protected boolean optional = OPTIONAL_EDEFAULT;

	/**
	 * The cached value of the '{@link #getColumn() <em>Column</em>}' reference
	 * @see #getColumn()
	 * @generated
	 * @ordered
	 */
	protected DBColumn column;

	/**
	 * @generated
	 */
	protected OneToOneAssociationImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DomainPackage.Literals.ONE_TO_ONE_ASSOCIATION;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.OneToOneAssociation#isOptional()
	 * @generated
	 */
	@Override
	public boolean isOptional() {
		return optional;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.OneToOneAssociation#setOptional(boolean)
	 * @generated
	 */
	@Override
	public void setOptional(boolean newOptional) {
		final boolean oldOptional = optional;
		optional = newOptional;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, DomainPackage.ONE_TO_ONE_ASSOCIATION__OPTIONAL, oldOptional, optional));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.OneToOneAssociation#getColumn()
	 * @generated
	 */
	@Override
	public DBColumn getColumn() {
		if (column != null && column.eIsProxy()) {
			final var oldColumn = (InternalEObject) column;
			column = (DBColumn) eResolveProxy(oldColumn);

			if (column != oldColumn && eNotificationRequired())
				eNotify(
						new ENotificationImpl(this, Notification.RESOLVE, DomainPackage.ONE_TO_ONE_ASSOCIATION__COLUMN, oldColumn, column));
		}

		return column;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DBColumn basicGetColumn() {
		return column;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.OneToOneAssociation#setColumn(net.codecadenza.eclipse.model.db.DBColumn)
	 * @generated
	 */
	@Override
	public void setColumn(DBColumn newColumn) {
		final DBColumn oldColumn = column;
		column = newColumn;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ONE_TO_ONE_ASSOCIATION__COLUMN, oldColumn, column));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DomainPackage.ONE_TO_ONE_ASSOCIATION__OPTIONAL:
				return isOptional();
			case DomainPackage.ONE_TO_ONE_ASSOCIATION__COLUMN:
				if (resolve)
					return getColumn();

				return basicGetColumn();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case DomainPackage.ONE_TO_ONE_ASSOCIATION__OPTIONAL:
				setOptional((Boolean) newValue);
				return;
			case DomainPackage.ONE_TO_ONE_ASSOCIATION__COLUMN:
				setColumn((DBColumn) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case DomainPackage.ONE_TO_ONE_ASSOCIATION__OPTIONAL:
				setOptional(OPTIONAL_EDEFAULT);
				return;
			case DomainPackage.ONE_TO_ONE_ASSOCIATION__COLUMN:
				setColumn((DBColumn) null);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case DomainPackage.ONE_TO_ONE_ASSOCIATION__OPTIONAL:
				return optional != OPTIONAL_EDEFAULT;
			case DomainPackage.ONE_TO_ONE_ASSOCIATION__COLUMN:
				return column != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.OneToOneAssociation#isBidirectional()
	 * @generated
	 */
	@Override
	public boolean isBidirectional() {
		return getReverseAssociation() != null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getReverseAssociation()
	 * @generated not
	 */
	@Override
	public OneToOneAssociation getReverseAssociation() {
		return (OneToOneAssociation) super.getReverseAssociation();
	}

}
