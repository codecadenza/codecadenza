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
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>Many To One Association</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.ManyToOneAssociationImpl#isInsertable <em>Insertable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.ManyToOneAssociationImpl#isUpdatable <em>Updatable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.ManyToOneAssociationImpl#isOptional <em>Optional</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.ManyToOneAssociationImpl#getColumn <em>Column</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class ManyToOneAssociationImpl extends AbstractDomainAssociationImpl implements ManyToOneAssociation {
	/**
	 * The default value of the '{@link #isInsertable() <em>Insertable</em>}' attribute
	 * @see #isInsertable()
	 * @generated
	 * @ordered
	 */
	protected static final boolean INSERTABLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isInsertable() <em>Insertable</em>}' attribute
	 * @see #isInsertable()
	 * @generated
	 * @ordered
	 */
	protected boolean insertable = INSERTABLE_EDEFAULT;

	/**
	 * The default value of the '{@link #isUpdatable() <em>Updatable</em>}' attribute
	 * @see #isUpdatable()
	 * @generated
	 * @ordered
	 */
	protected static final boolean UPDATABLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isUpdatable() <em>Updatable</em>}' attribute
	 * @see #isUpdatable()
	 * @generated
	 * @ordered
	 */
	protected boolean updatable = UPDATABLE_EDEFAULT;

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
	protected ManyToOneAssociationImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DomainPackage.Literals.MANY_TO_ONE_ASSOCIATION;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.ManyToOneAssociation#isInsertable()
	 * @generated
	 */
	@Override
	public boolean isInsertable() {
		return insertable;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.ManyToOneAssociation#setInsertable(boolean)
	 * @generated
	 */
	@Override
	public void setInsertable(boolean newInsertable) {
		final boolean oldInsertable = insertable;
		insertable = newInsertable;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.MANY_TO_ONE_ASSOCIATION__INSERTABLE, oldInsertable,
					insertable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.ManyToOneAssociation#isUpdatable()
	 * @generated
	 */
	@Override
	public boolean isUpdatable() {
		return updatable;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.ManyToOneAssociation#setUpdatable(boolean)
	 * @generated
	 */
	@Override
	public void setUpdatable(boolean newUpdatable) {
		final boolean oldUpdatable = updatable;
		updatable = newUpdatable;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.MANY_TO_ONE_ASSOCIATION__UPDATABLE, oldUpdatable,
					updatable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.ManyToOneAssociation#isOptional()
	 * @generated
	 */
	@Override
	public boolean isOptional() {
		return optional;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.ManyToOneAssociation#setOptional(boolean)
	 * @generated
	 */
	@Override
	public void setOptional(boolean newOptional) {
		final boolean oldOptional = optional;
		optional = newOptional;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, DomainPackage.MANY_TO_ONE_ASSOCIATION__OPTIONAL, oldOptional, optional));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.ManyToOneAssociation#getColumn()
	 * @generated
	 */
	@Override
	public DBColumn getColumn() {
		if (column != null && column.eIsProxy()) {
			final var oldColumn = (InternalEObject) column;
			column = (DBColumn) eResolveProxy(oldColumn);

			if (column != oldColumn && eNotificationRequired())
				eNotify(
						new ENotificationImpl(this, Notification.RESOLVE, DomainPackage.MANY_TO_ONE_ASSOCIATION__COLUMN, oldColumn, column));
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
	 * @see net.codecadenza.eclipse.model.domain.ManyToOneAssociation#setColumn(net.codecadenza.eclipse.model.db.DBColumn)
	 * @generated
	 */
	@Override
	public void setColumn(DBColumn newColumn) {
		final DBColumn oldColumn = column;
		column = newColumn;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.MANY_TO_ONE_ASSOCIATION__COLUMN, oldColumn, column));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DomainPackage.MANY_TO_ONE_ASSOCIATION__INSERTABLE:
				return isInsertable();
			case DomainPackage.MANY_TO_ONE_ASSOCIATION__UPDATABLE:
				return isUpdatable();
			case DomainPackage.MANY_TO_ONE_ASSOCIATION__OPTIONAL:
				return isOptional();
			case DomainPackage.MANY_TO_ONE_ASSOCIATION__COLUMN:
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
			case DomainPackage.MANY_TO_ONE_ASSOCIATION__INSERTABLE:
				setInsertable((Boolean) newValue);
				return;
			case DomainPackage.MANY_TO_ONE_ASSOCIATION__UPDATABLE:
				setUpdatable((Boolean) newValue);
				return;
			case DomainPackage.MANY_TO_ONE_ASSOCIATION__OPTIONAL:
				setOptional((Boolean) newValue);
				return;
			case DomainPackage.MANY_TO_ONE_ASSOCIATION__COLUMN:
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
			case DomainPackage.MANY_TO_ONE_ASSOCIATION__INSERTABLE:
				setInsertable(INSERTABLE_EDEFAULT);
				return;
			case DomainPackage.MANY_TO_ONE_ASSOCIATION__UPDATABLE:
				setUpdatable(UPDATABLE_EDEFAULT);
				return;
			case DomainPackage.MANY_TO_ONE_ASSOCIATION__OPTIONAL:
				setOptional(OPTIONAL_EDEFAULT);
				return;
			case DomainPackage.MANY_TO_ONE_ASSOCIATION__COLUMN:
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
			case DomainPackage.MANY_TO_ONE_ASSOCIATION__INSERTABLE:
				return insertable != INSERTABLE_EDEFAULT;
			case DomainPackage.MANY_TO_ONE_ASSOCIATION__UPDATABLE:
				return updatable != UPDATABLE_EDEFAULT;
			case DomainPackage.MANY_TO_ONE_ASSOCIATION__OPTIONAL:
				return optional != OPTIONAL_EDEFAULT;
			case DomainPackage.MANY_TO_ONE_ASSOCIATION__COLUMN:
				return column != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (insertable: ");
		result.append(insertable);
		result.append(", updatable: ");
		result.append(updatable);
		result.append(", optional: ");
		result.append(optional);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.ManyToOneAssociation#getReverseAssociation()
	 * @generated not
	 */
	@Override
	public OneToManyAssociation getReverseAssociation() {
		for (final AbstractDomainAssociation assoc : target.getAssociations()) {
			if (!(assoc instanceof final OneToManyAssociation otm))
				continue;

			if (this.equals(otm.getReverseAssociation()))
				return otm;
		}

		return null;
	}

}
