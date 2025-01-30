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

import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.util.JavaBeanHelper;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>Abstract Domain Association</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#isOwner <em>Owner</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#isCascadePersist <em>Cascade
 * Persist</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#isCascadeMerge <em>Cascade Merge</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#isCascadeRemove <em>Cascade Remove</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#isCascadeRefresh <em>Cascade
 * Refresh</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#isFetchTypeEager <em>Fetch Type
 * Eager</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#getDomainObject <em>Domain Object</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#getTarget <em>Target</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#getTag <em>Tag</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#getInternalComment <em>Internal
 * Comment</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#getUserComment <em>User Comment</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl#getReverseAssociation <em>Reverse
 * association</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public abstract class AbstractDomainAssociationImpl extends EObjectImpl implements AbstractDomainAssociation {
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
	 * The default value of the '{@link #isOwner() <em>Owner</em>}' attribute
	 * @see #isOwner()
	 * @generated
	 * @ordered
	 */
	protected static final boolean OWNER_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isOwner() <em>Owner</em>}' attribute
	 * @see #isOwner()
	 * @generated
	 * @ordered
	 */
	protected boolean owner = OWNER_EDEFAULT;

	/**
	 * The default value of the '{@link #isCascadePersist() <em>Cascade Persist</em>}' attribute
	 * @see #isCascadePersist()
	 * @generated
	 * @ordered
	 */
	protected static final boolean CASCADE_PERSIST_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isCascadePersist() <em>Cascade Persist</em>}' attribute
	 * @see #isCascadePersist()
	 * @generated
	 * @ordered
	 */
	protected boolean cascadePersist = CASCADE_PERSIST_EDEFAULT;

	/**
	 * The default value of the '{@link #isCascadeMerge() <em>Cascade Merge</em>}' attribute
	 * @see #isCascadeMerge()
	 * @generated
	 * @ordered
	 */
	protected static final boolean CASCADE_MERGE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isCascadeMerge() <em>Cascade Merge</em>}' attribute
	 * @see #isCascadeMerge()
	 * @generated
	 * @ordered
	 */
	protected boolean cascadeMerge = CASCADE_MERGE_EDEFAULT;

	/**
	 * The default value of the '{@link #isCascadeRemove() <em>Cascade Remove</em>}' attribute
	 * @see #isCascadeRemove()
	 * @generated
	 * @ordered
	 */
	protected static final boolean CASCADE_REMOVE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isCascadeRemove() <em>Cascade Remove</em>}' attribute
	 * @see #isCascadeRemove()
	 * @generated
	 * @ordered
	 */
	protected boolean cascadeRemove = CASCADE_REMOVE_EDEFAULT;

	/**
	 * The default value of the '{@link #isCascadeRefresh() <em>Cascade Refresh</em>}' attribute
	 * @see #isCascadeRefresh()
	 * @generated
	 * @ordered
	 */
	protected static final boolean CASCADE_REFRESH_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isCascadeRefresh() <em>Cascade Refresh</em>}' attribute
	 * @see #isCascadeRefresh()
	 * @generated
	 * @ordered
	 */
	protected boolean cascadeRefresh = CASCADE_REFRESH_EDEFAULT;

	/**
	 * The default value of the '{@link #isFetchTypeEager() <em>Fetch Type Eager</em>}' attribute
	 * @see #isFetchTypeEager()
	 * @generated
	 * @ordered
	 */
	protected static final boolean FETCH_TYPE_EAGER_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isFetchTypeEager() <em>Fetch Type Eager</em>}' attribute
	 * @see #isFetchTypeEager()
	 * @generated
	 * @ordered
	 */
	protected boolean fetchTypeEager = FETCH_TYPE_EAGER_EDEFAULT;

	/**
	 * The cached value of the '{@link #getTarget() <em>Target</em>}' reference
	 * @see #getTarget()
	 * @generated
	 * @ordered
	 */
	protected DomainObject target;

	/**
	 * The default value of the '{@link #getTag() <em>Tag</em>}' attribute
	 * @see #getTag()
	 * @generated
	 * @ordered
	 */
	protected static final AssociationTagEnumeration TAG_EDEFAULT = AssociationTagEnumeration.NONE;

	/**
	 * The cached value of the '{@link #getTag() <em>Tag</em>}' attribute
	 * @see #getTag()
	 * @generated
	 * @ordered
	 */
	protected AssociationTagEnumeration tag = TAG_EDEFAULT;

	/**
	 * The default value of the '{@link #getInternalComment() <em>Internal Comment</em>}' attribute
	 * @see #getInternalComment()
	 * @generated
	 * @ordered
	 */
	protected static final String INTERNAL_COMMENT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getInternalComment() <em>Internal Comment</em>}' attribute
	 * @see #getInternalComment()
	 * @generated
	 * @ordered
	 */
	protected String internalComment = INTERNAL_COMMENT_EDEFAULT;

	/**
	 * The default value of the '{@link #getUserComment() <em>User Comment</em>}' attribute
	 * @see #getUserComment()
	 * @generated
	 * @ordered
	 */
	protected static final String USER_COMMENT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getUserComment() <em>User Comment</em>}' attribute
	 * @see #getUserComment()
	 * @generated
	 * @ordered
	 */
	protected String userComment = USER_COMMENT_EDEFAULT;

	/**
	 * The cached value of the '{@link #getReverseAssociation() <em>Reverse Association</em>}' reference
	 * @see #getReverseAssociation()
	 * @generated
	 * @ordered
	 */
	protected AbstractDomainAssociation reverseAssociation;

	/**
	 * @generated
	 */
	protected AbstractDomainAssociationImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DomainPackage.Literals.ABSTRACT_DOMAIN_ASSOCIATION;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isOwner()
	 */
	@Override
	public boolean isOwner() {
		return owner;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#setOwner(boolean)
	 * @generated
	 */
	@Override
	public void setOwner(boolean newOwner) {
		final boolean oldOwner = owner;
		owner = newOwner;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__OWNER, oldOwner, owner));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadePersist()
	 * @generated
	 */
	@Override
	public boolean isCascadePersist() {
		return cascadePersist;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#setCascadePersist(boolean)
	 * @generated
	 */
	@Override
	public void setCascadePersist(boolean newCascadePersist) {
		final boolean oldCascadePersist = cascadePersist;
		cascadePersist = newCascadePersist;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_PERSIST,
					oldCascadePersist, cascadePersist));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadeMerge()
	 * @generated
	 */
	@Override
	public boolean isCascadeMerge() {
		return cascadeMerge;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#setCascadeMerge(boolean)
	 * @generated
	 */
	@Override
	public void setCascadeMerge(boolean newCascadeMerge) {
		final boolean oldCascadeMerge = cascadeMerge;
		cascadeMerge = newCascadeMerge;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_MERGE,
					oldCascadeMerge, cascadeMerge));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadeRemove()
	 * @generated
	 */
	@Override
	public boolean isCascadeRemove() {
		return cascadeRemove;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#setCascadeRemove(boolean)
	 * @generated
	 */
	@Override
	public void setCascadeRemove(boolean newCascadeRemove) {
		final boolean oldCascadeRemove = cascadeRemove;
		cascadeRemove = newCascadeRemove;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REMOVE,
					oldCascadeRemove, cascadeRemove));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadeRefresh()
	 * @generated
	 */
	@Override
	public boolean isCascadeRefresh() {
		return cascadeRefresh;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#setCascadeRefresh(boolean)
	 * @generated
	 */
	@Override
	public void setCascadeRefresh(boolean newCascadeRefresh) {
		final boolean oldCascadeRefresh = cascadeRefresh;
		cascadeRefresh = newCascadeRefresh;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REFRESH,
					oldCascadeRefresh, cascadeRefresh));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isFetchTypeEager()
	 * @generated
	 */
	@Override
	public boolean isFetchTypeEager() {
		return fetchTypeEager;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#setFetchTypeEager(boolean)
	 * @generated
	 */
	@Override
	public void setFetchTypeEager(boolean newFetchTypeEager) {
		final boolean oldFetchTypeEager = fetchTypeEager;
		fetchTypeEager = newFetchTypeEager;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__FETCH_TYPE_EAGER,
					oldFetchTypeEager, fetchTypeEager));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getDomainObject()
	 * @generated
	 */
	@Override
	public DomainObject getDomainObject() {
		if (eContainerFeatureID() != DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT)
			return null;

		return (DomainObject) eInternalContainer();
	}

	/**
	 * @param newDomainObject
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetDomainObject(DomainObject newDomainObject, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newDomainObject, DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#setDomainObject(net.codecadenza.eclipse.model.domain.
	 * DomainObject)
	 * @generated
	 */
	@Override
	public void setDomainObject(DomainObject newDomainObject) {
		if (newDomainObject != eInternalContainer()
				|| (eContainerFeatureID() != DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT && newDomainObject != null)) {
			if (EcoreUtil.isAncestor(this, newDomainObject))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newDomainObject != null)
				msgs = ((InternalEObject) newDomainObject).eInverseAdd(this, DomainPackage.DOMAIN_OBJECT__ASSOCIATIONS,
						DomainObject.class, msgs);

			msgs = basicSetDomainObject(newDomainObject, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT,
					newDomainObject, newDomainObject));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getTarget()
	 * @generated
	 */
	@Override
	public DomainObject getTarget() {
		if (target != null && target.eIsProxy()) {
			final var oldTarget = (InternalEObject) target;
			target = (DomainObject) eResolveProxy(oldTarget);

			if (target != oldTarget && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__TARGET, oldTarget,
						target));
		}

		return target;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DomainObject basicGetTarget() {
		return target;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#setTarget(net.codecadenza.eclipse.model.domain.
	 * DomainObject)
	 * @generated
	 */
	@Override
	public void setTarget(DomainObject newTarget) {
		final DomainObject oldTarget = target;
		target = newTarget;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__TARGET, oldTarget, newTarget));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getTag()
	 * @generated
	 */
	@Override
	public AssociationTagEnumeration getTag() {
		return tag;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#setTag(net.codecadenza.eclipse.model.domain.
	 * AssociationTagEnumeration)
	 * @generated
	 */
	@Override
	public void setTag(AssociationTagEnumeration newTag) {
		final AssociationTagEnumeration oldTag = tag;
		tag = newTag == null ? TAG_EDEFAULT : newTag;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__TAG, oldTag, tag));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getInternalComment()
	 * @generated
	 */
	@Override
	public String getInternalComment() {
		return internalComment;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#setInternalComment(java.lang.String)
	 * @generated
	 */
	@Override
	public void setInternalComment(String newInternalComment) {
		final String oldInternalComment = internalComment;
		internalComment = newInternalComment;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__INTERNAL_COMMENT,
					oldInternalComment, internalComment));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getUserComment()
	 * @generated
	 */
	@Override
	public String getUserComment() {
		return userComment;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#setUserComment(java.lang.String)
	 * @generated
	 */
	@Override
	public void setUserComment(String newUserComment) {
		final String oldUserComment = userComment;
		userComment = newUserComment;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__USER_COMMENT,
					oldUserComment, userComment));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getReverseAssociation()
	 * @generated
	 */
	@Override
	public AbstractDomainAssociation getReverseAssociation() {
		if (reverseAssociation != null && reverseAssociation.eIsProxy()) {
			final InternalEObject oldReverseAssociation = (InternalEObject) reverseAssociation;
			reverseAssociation = (AbstractDomainAssociation) eResolveProxy(oldReverseAssociation);

			if ((reverseAssociation != oldReverseAssociation) && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__REVERSE_ASSOCIATION,
						oldReverseAssociation, reverseAssociation));
		}

		return reverseAssociation;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public AbstractDomainAssociation basicGetReverseAssociation() {
		return reverseAssociation;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#setReverseAssociation(net.codecadenza.eclipse.model.domain.
	 * AbstractDomainAssociation)
	 * @generated
	 */
	@Override
	public void setReverseAssociation(AbstractDomainAssociation newReverseAssociation) {
		final AbstractDomainAssociation oldReverseAssociation = reverseAssociation;
		reverseAssociation = newReverseAssociation;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__REVERSE_ASSOCIATION,
					oldReverseAssociation, reverseAssociation));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetDomainObject((DomainObject) otherEnd, msgs);
		}

		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT:
				return basicSetDomainObject(null, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eBasicRemoveFromContainerFeature(org.eclipse.emf.common.notify.
	 * NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eBasicRemoveFromContainerFeature(NotificationChain msgs) {
		switch (eContainerFeatureID()) {
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT:
				return eInternalContainer().eInverseRemove(this, DomainPackage.DOMAIN_OBJECT__ASSOCIATIONS, DomainObject.class, msgs);
		}

		return super.eBasicRemoveFromContainerFeature(msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__NAME:
				return getName();
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__OWNER:
				return isOwner();
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_PERSIST:
				return isCascadePersist();
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_MERGE:
				return isCascadeMerge();
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REMOVE:
				return isCascadeRemove();
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REFRESH:
				return isCascadeRefresh();
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__FETCH_TYPE_EAGER:
				return isFetchTypeEager();
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT:
				return getDomainObject();
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__TARGET:
				if (resolve)
					return getTarget();

				return basicGetTarget();
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__TAG:
				return getTag();
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__INTERNAL_COMMENT:
				return getInternalComment();
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__USER_COMMENT:
				return getUserComment();
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__REVERSE_ASSOCIATION:
				return getReverseAssociation();
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
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__NAME:
				setName((String) newValue);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__OWNER:
				setOwner((Boolean) newValue);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_PERSIST:
				setCascadePersist((Boolean) newValue);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_MERGE:
				setCascadeMerge((Boolean) newValue);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REMOVE:
				setCascadeRemove((Boolean) newValue);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REFRESH:
				setCascadeRefresh((Boolean) newValue);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__FETCH_TYPE_EAGER:
				setFetchTypeEager((Boolean) newValue);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT:
				setDomainObject((DomainObject) newValue);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__TARGET:
				setTarget((DomainObject) newValue);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__TAG:
				setTag((AssociationTagEnumeration) newValue);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__INTERNAL_COMMENT:
				setInternalComment((String) newValue);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__USER_COMMENT:
				setUserComment((String) newValue);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__REVERSE_ASSOCIATION:
				setReverseAssociation((AbstractDomainAssociation) newValue);
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
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__NAME:
				setName(NAME_EDEFAULT);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__OWNER:
				setOwner(OWNER_EDEFAULT);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_PERSIST:
				setCascadePersist(CASCADE_PERSIST_EDEFAULT);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_MERGE:
				setCascadeMerge(CASCADE_MERGE_EDEFAULT);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REMOVE:
				setCascadeRemove(CASCADE_REMOVE_EDEFAULT);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REFRESH:
				setCascadeRefresh(CASCADE_REFRESH_EDEFAULT);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__FETCH_TYPE_EAGER:
				setFetchTypeEager(FETCH_TYPE_EAGER_EDEFAULT);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT:
				setDomainObject((DomainObject) null);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__TARGET:
				setTarget((DomainObject) null);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__TAG:
				setTag(TAG_EDEFAULT);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__INTERNAL_COMMENT:
				setInternalComment(INTERNAL_COMMENT_EDEFAULT);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__USER_COMMENT:
				setUserComment(USER_COMMENT_EDEFAULT);
				return;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__REVERSE_ASSOCIATION:
				setReverseAssociation(null);
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
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__NAME:
				return name != null;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__OWNER:
				return owner != OWNER_EDEFAULT;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_PERSIST:
				return cascadePersist != CASCADE_PERSIST_EDEFAULT;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_MERGE:
				return cascadeMerge != CASCADE_MERGE_EDEFAULT;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REMOVE:
				return cascadeRemove != CASCADE_REMOVE_EDEFAULT;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REFRESH:
				return cascadeRefresh != CASCADE_REFRESH_EDEFAULT;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__FETCH_TYPE_EAGER:
				return fetchTypeEager != FETCH_TYPE_EAGER_EDEFAULT;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT:
				return getDomainObject() != null;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__TARGET:
				return target != null;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__TAG:
				return tag != TAG_EDEFAULT;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__INTERNAL_COMMENT:
				return internalComment != null;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__USER_COMMENT:
				return userComment != null;
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__REVERSE_ASSOCIATION:
				return reverseAssociation != null;
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
		result.append(", owner: ");
		result.append(owner);
		result.append(", cascadePersist: ");
		result.append(cascadePersist);
		result.append(", cascadeMerge: ");
		result.append(cascadeMerge);
		result.append(", cascadeRemove: ");
		result.append(cascadeRemove);
		result.append(", cascadeRefresh: ");
		result.append(cascadeRefresh);
		result.append(", fetchTypeEager: ");
		result.append(fetchTypeEager);
		result.append(", tag: ");
		result.append(tag);
		result.append(", internalComment: ");
		result.append(internalComment);
		result.append(", userComment: ");
		result.append(userComment);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getUpperCaseName()
	 * @generated not
	 */
	@Override
	public String getUpperCaseName() {
		if (name.isEmpty())
			return "";

		if (name.length() == 1)
			return name.toUpperCase();

		return name.substring(0, 1).toUpperCase() + name.substring(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getGetterName()
	 * @generated not
	 */
	@Override
	public String getGetterName() {
		return JavaBeanHelper.getGetterName(name, false);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getSetterName()
	 * @generated not
	 */
	@Override
	public String getSetterName() {
		return JavaBeanHelper.getSetterName(name);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getGUILabel()
	 * @generated not
	 */
	@Override
	public String getGUILabel() {
		final var b = new StringBuilder();

		if (name.isEmpty())
			return b.toString();

		for (final char c : name.substring(1).toCharArray())
			if (c == Character.toUpperCase(c))
				b.append(" " + Character.toLowerCase(c));
			else
				b.append(c);

		return name.substring(0, 1).toUpperCase() + b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getGetterReference()
	 * @generated not
	 */
	@Override
	public String getGetterReference() {
		return JavaBeanHelper.getGetterReference(name, false);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getSetterReference()
	 * @generated not
	 */
	@Override
	public String getSetterReference() {
		return "::" + getSetterName();
	}

}
