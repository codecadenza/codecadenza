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

import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>One To Many Association</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.OneToManyAssociationImpl#getTable <em>Table</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class OneToManyAssociationImpl extends AbstractDomainAssociationImpl implements OneToManyAssociation {
	/**
	 * The cached value of the '{@link #getTable() <em>Table</em>}' containment reference
	 * @see #getTable()
	 * @generated
	 * @ordered
	 */
	protected DBTable table;

	/**
	 * @generated
	 */
	protected OneToManyAssociationImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DomainPackage.Literals.ONE_TO_MANY_ASSOCIATION;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.OneToManyAssociation#getTable()
	 * @generated
	 */
	@Override
	public DBTable getTable() {
		return table;
	}

	/**
	 * @param newTable
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetTable(DBTable newTable, NotificationChain msgs) {
		final DBTable oldTable = table;
		table = newTable;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, DomainPackage.ONE_TO_MANY_ASSOCIATION__TABLE,
					oldTable, newTable);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.OneToManyAssociation#setTable(net.codecadenza.eclipse.model.db.DBTable)
	 * @generated
	 */
	@Override
	public void setTable(DBTable newTable) {
		if (newTable != table) {
			NotificationChain msgs = null;

			if (table != null)
				msgs = ((InternalEObject) table).eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE - DomainPackage.ONE_TO_MANY_ASSOCIATION__TABLE, null, msgs);

			if (newTable != null)
				msgs = ((InternalEObject) newTable).eInverseAdd(this,
						EOPPOSITE_FEATURE_BASE - DomainPackage.ONE_TO_MANY_ASSOCIATION__TABLE, null, msgs);

			msgs = basicSetTable(newTable, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ONE_TO_MANY_ASSOCIATION__TABLE, newTable, newTable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#eInverseRemove(org.eclipse.emf.ecore.
	 * InternalEObject, int, org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case DomainPackage.ONE_TO_MANY_ASSOCIATION__TABLE:
				return basicSetTable(null, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DomainPackage.ONE_TO_MANY_ASSOCIATION__TABLE:
				return getTable();
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
			case DomainPackage.ONE_TO_MANY_ASSOCIATION__TABLE:
				setTable((DBTable) newValue);
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
			case DomainPackage.ONE_TO_MANY_ASSOCIATION__TABLE:
				setTable((DBTable) null);
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
			case DomainPackage.ONE_TO_MANY_ASSOCIATION__TABLE:
				return table != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.OneToManyAssociation#isBidirectional()
	 * @generated
	 */
	@Override
	public boolean isBidirectional() {
		return getReverseAssociation() != null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.OneToManyAssociation#getReverseAssociation()
	 * @generated not
	 */
	@Override
	public ManyToOneAssociation getReverseAssociation() {
		return (ManyToOneAssociation) super.getReverseAssociation();
	}

}
