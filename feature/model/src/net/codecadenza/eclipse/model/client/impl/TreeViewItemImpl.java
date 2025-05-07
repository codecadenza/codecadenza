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
package net.codecadenza.eclipse.model.client.impl;

import java.util.Collection;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.client.TreeNode;
import net.codecadenza.eclipse.model.client.TreeViewItem;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Tree View Item</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TreeViewItemImpl#getParentItem <em>Parent Item</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TreeViewItemImpl#getChildren <em>Children</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TreeViewItemImpl#getDataFetchMethod <em>Data Fetch Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TreeViewItemImpl#getDropMethod <em>Drop Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TreeViewItemImpl#getDisplayAttributes <em>Display Attributes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TreeViewItemImpl#getAssociation <em>Association</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TreeViewItemImpl#getNodes <em>Nodes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TreeViewItemImpl#getLabel <em>Label</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TreeViewItemImpl#getItemDTO <em>Item DTO</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TreeViewItemImpl#getInvisibleAttributes <em>Invisible
 * Attributes</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class TreeViewItemImpl extends EObjectImpl implements TreeViewItem {
	/**
	 * The cached value of the '{@link #getChildren() <em>Children</em>}' containment reference list
	 * @see #getChildren()
	 * @generated
	 * @ordered
	 */
	protected EList<TreeViewItem> children;

	/**
	 * The cached value of the '{@link #getDataFetchMethod() <em>Data Fetch Method</em>}' reference
	 * @see #getDataFetchMethod()
	 * @generated
	 * @ordered
	 */
	protected BoundaryMethod dataFetchMethod;

	/**
	 * The cached value of the '{@link #getDropMethod() <em>Drop Method</em>}' reference
	 * @see #getDropMethod()
	 * @generated
	 * @ordered
	 */
	protected BoundaryMethod dropMethod;

	/**
	 * The cached value of the '{@link #getDisplayAttributes() <em>Display Attributes</em>}' reference list
	 * @see #getDisplayAttributes()
	 * @generated
	 * @ordered
	 */
	protected EList<DTOBeanAttribute> displayAttributes;

	/**
	 * The cached value of the '{@link #getAssociation() <em>Association</em>}' reference
	 * @see #getAssociation()
	 * @generated
	 * @ordered
	 */
	protected AbstractDomainAssociation association;

	/**
	 * The cached value of the '{@link #getNodes() <em>Nodes</em>}' containment reference list
	 * @see #getNodes()
	 * @generated
	 * @ordered
	 */
	protected EList<TreeNode> nodes;

	/**
	 * The default value of the '{@link #getLabel() <em>Label</em>}' attribute
	 * @see #getLabel()
	 * @generated
	 * @ordered
	 */
	protected static final String LABEL_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getLabel() <em>Label</em>}' attribute
	 * @see #getLabel()
	 * @generated
	 * @ordered
	 */
	protected String label = LABEL_EDEFAULT;

	/**
	 * The cached value of the '{@link #getItemDTO() <em>Item DTO</em>}' reference
	 * @see #getItemDTO()
	 * @generated
	 * @ordered
	 */
	protected DTOBean itemDTO;

	/**
	 * The cached value of the '{@link #getInvisibleAttributes() <em>Invisible Attributes</em>}' reference list
	 * @see #getInvisibleAttributes()
	 * @generated
	 * @ordered
	 */
	protected EList<DTOBeanAttribute> invisibleAttributes;

	/**
	 * @generated
	 */
	protected TreeViewItemImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ClientPackage.Literals.TREE_VIEW_ITEM;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getParentItem()
	 * @generated
	 */
	@Override
	public TreeViewItem getParentItem() {
		if (eContainerFeatureID() != ClientPackage.TREE_VIEW_ITEM__PARENT_ITEM)
			return null;

		return (TreeViewItem) eInternalContainer();
	}

	/**
	 * @param newParentItem
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetParentItem(TreeViewItem newParentItem, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newParentItem, ClientPackage.TREE_VIEW_ITEM__PARENT_ITEM, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#setParentItem(net.codecadenza.eclipse.model.client.TreeViewItem)
	 * @generated
	 */
	@Override
	public void setParentItem(TreeViewItem newParentItem) {
		if (newParentItem != eInternalContainer()
				|| (eContainerFeatureID() != ClientPackage.TREE_VIEW_ITEM__PARENT_ITEM && newParentItem != null)) {
			if (EcoreUtil.isAncestor(this, newParentItem))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newParentItem != null)
				msgs = ((InternalEObject) newParentItem).eInverseAdd(this, ClientPackage.TREE_VIEW_ITEM__CHILDREN, TreeViewItem.class,
						msgs);

			msgs = basicSetParentItem(newParentItem, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ClientPackage.TREE_VIEW_ITEM__PARENT_ITEM, newParentItem, newParentItem));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getChildren()
	 * @generated
	 */
	@Override
	public EList<TreeViewItem> getChildren() {
		if (children == null)
			children = new EObjectContainmentWithInverseEList<>(TreeViewItem.class, this, ClientPackage.TREE_VIEW_ITEM__CHILDREN,
					ClientPackage.TREE_VIEW_ITEM__PARENT_ITEM);

		return children;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getDataFetchMethod()
	 * @generated
	 */
	@Override
	public BoundaryMethod getDataFetchMethod() {
		if (dataFetchMethod != null && dataFetchMethod.eIsProxy()) {
			final var oldDataFetchMethod = (InternalEObject) dataFetchMethod;
			dataFetchMethod = (BoundaryMethod) eResolveProxy(oldDataFetchMethod);

			if (dataFetchMethod != oldDataFetchMethod && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.TREE_VIEW_ITEM__DATA_FETCH_METHOD,
						oldDataFetchMethod, dataFetchMethod));
		}

		return dataFetchMethod;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public BoundaryMethod basicGetDataFetchMethod() {
		return dataFetchMethod;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#setDataFetchMethod(net.codecadenza.eclipse.model.boundary.
	 * BoundaryMethod)
	 * @generated
	 */
	@Override
	public void setDataFetchMethod(BoundaryMethod newDataFetchMethod) {
		final BoundaryMethod oldDataFetchMethod = dataFetchMethod;
		dataFetchMethod = newDataFetchMethod;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.TREE_VIEW_ITEM__DATA_FETCH_METHOD, oldDataFetchMethod,
					dataFetchMethod));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getDropMethod()
	 * @generated
	 */
	@Override
	public BoundaryMethod getDropMethod() {
		if (dropMethod != null && dropMethod.eIsProxy()) {
			final var oldDropMethod = (InternalEObject) dropMethod;
			dropMethod = (BoundaryMethod) eResolveProxy(oldDropMethod);

			if (dropMethod != oldDropMethod && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.TREE_VIEW_ITEM__DROP_METHOD, oldDropMethod,
						dropMethod));
		}

		return dropMethod;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public BoundaryMethod basicGetDropMethod() {
		return dropMethod;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#setDropMethod(net.codecadenza.eclipse.model.boundary.BoundaryMethod)
	 * @generated
	 */
	@Override
	public void setDropMethod(BoundaryMethod newDropMethod) {
		final BoundaryMethod oldDropMethod = dropMethod;
		dropMethod = newDropMethod;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ClientPackage.TREE_VIEW_ITEM__DROP_METHOD, oldDropMethod, dropMethod));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getDisplayAttributes()
	 * @generated
	 */
	@Override
	public EList<DTOBeanAttribute> getDisplayAttributes() {
		if (displayAttributes == null)
			displayAttributes = new EObjectResolvingEList<>(DTOBeanAttribute.class, this,
					ClientPackage.TREE_VIEW_ITEM__DISPLAY_ATTRIBUTES);

		return displayAttributes;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getAssociation()
	 * @generated
	 */
	@Override
	public AbstractDomainAssociation getAssociation() {
		if (association != null && association.eIsProxy()) {
			final var oldAssociation = (InternalEObject) association;
			association = (AbstractDomainAssociation) eResolveProxy(oldAssociation);

			if (association != oldAssociation && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.TREE_VIEW_ITEM__ASSOCIATION, oldAssociation,
						association));
		}

		return association;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public AbstractDomainAssociation basicGetAssociation() {
		return association;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#setAssociation(net.codecadenza.eclipse.model.domain.
	 * AbstractDomainAssociation)
	 * @generated
	 */
	@Override
	public void setAssociation(AbstractDomainAssociation newAssociation) {
		final AbstractDomainAssociation oldAssociation = association;
		association = newAssociation;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, ClientPackage.TREE_VIEW_ITEM__ASSOCIATION, oldAssociation, association));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getNodes()
	 * @generated
	 */
	@Override
	public EList<TreeNode> getNodes() {
		if (nodes == null)
			nodes = new EObjectContainmentEList<>(TreeNode.class, this, ClientPackage.TREE_VIEW_ITEM__NODES);

		return nodes;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getLabel()
	 * @generated
	 */
	@Override
	public String getLabel() {
		return label;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#setLabel(java.lang.String)
	 * @generated
	 */
	@Override
	public void setLabel(String newLabel) {
		final String oldLabel = label;
		label = newLabel;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.TREE_VIEW_ITEM__LABEL, oldLabel, label));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getItemDTO()
	 * @generated
	 */
	@Override
	public DTOBean getItemDTO() {
		if (itemDTO != null && itemDTO.eIsProxy()) {
			final var oldItemDTO = (InternalEObject) itemDTO;
			itemDTO = (DTOBean) eResolveProxy(oldItemDTO);

			if (itemDTO != oldItemDTO && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.TREE_VIEW_ITEM__ITEM_DTO, oldItemDTO, itemDTO));
		}

		return itemDTO;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DTOBean basicGetItemDTO() {
		return itemDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#setItemDTO(net.codecadenza.eclipse.model.dto.DTOBean)
	 * @generated
	 */
	@Override
	public void setItemDTO(DTOBean newItemDTO) {
		final DTOBean oldItemDTO = itemDTO;
		itemDTO = newItemDTO;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.TREE_VIEW_ITEM__ITEM_DTO, oldItemDTO, itemDTO));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getInvisibleAttributes()
	 * @generated
	 */
	@Override
	public EList<DTOBeanAttribute> getInvisibleAttributes() {
		if (invisibleAttributes == null)
			invisibleAttributes = new EObjectResolvingEList<>(DTOBeanAttribute.class, this,
					ClientPackage.TREE_VIEW_ITEM__INVISIBLE_ATTRIBUTES);

		return invisibleAttributes;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ClientPackage.TREE_VIEW_ITEM__PARENT_ITEM:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetParentItem((TreeViewItem) otherEnd, msgs);
			case ClientPackage.TREE_VIEW_ITEM__CHILDREN:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getChildren()).basicAdd(otherEnd, msgs);
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
			case ClientPackage.TREE_VIEW_ITEM__PARENT_ITEM:
				return basicSetParentItem(null, msgs);
			case ClientPackage.TREE_VIEW_ITEM__CHILDREN:
				return ((InternalEList<?>) getChildren()).basicRemove(otherEnd, msgs);
			case ClientPackage.TREE_VIEW_ITEM__NODES:
				return ((InternalEList<?>) getNodes()).basicRemove(otherEnd, msgs);
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
			case ClientPackage.TREE_VIEW_ITEM__PARENT_ITEM:
				return eInternalContainer().eInverseRemove(this, ClientPackage.TREE_VIEW_ITEM__CHILDREN, TreeViewItem.class, msgs);
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
			case ClientPackage.TREE_VIEW_ITEM__PARENT_ITEM:
				return getParentItem();
			case ClientPackage.TREE_VIEW_ITEM__CHILDREN:
				return getChildren();
			case ClientPackage.TREE_VIEW_ITEM__DATA_FETCH_METHOD:
				if (resolve)
					return getDataFetchMethod();

				return basicGetDataFetchMethod();
			case ClientPackage.TREE_VIEW_ITEM__DROP_METHOD:
				if (resolve)
					return getDropMethod();

				return basicGetDropMethod();
			case ClientPackage.TREE_VIEW_ITEM__DISPLAY_ATTRIBUTES:
				return getDisplayAttributes();
			case ClientPackage.TREE_VIEW_ITEM__ASSOCIATION:
				if (resolve)
					return getAssociation();

				return basicGetAssociation();
			case ClientPackage.TREE_VIEW_ITEM__NODES:
				return getNodes();
			case ClientPackage.TREE_VIEW_ITEM__LABEL:
				return getLabel();
			case ClientPackage.TREE_VIEW_ITEM__ITEM_DTO:
				if (resolve)
					return getItemDTO();

				return basicGetItemDTO();
			case ClientPackage.TREE_VIEW_ITEM__INVISIBLE_ATTRIBUTES:
				return getInvisibleAttributes();
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
			case ClientPackage.TREE_VIEW_ITEM__PARENT_ITEM:
				setParentItem((TreeViewItem) newValue);
				return;
			case ClientPackage.TREE_VIEW_ITEM__CHILDREN:
				getChildren().clear();
				getChildren().addAll((Collection<? extends TreeViewItem>) newValue);
				return;
			case ClientPackage.TREE_VIEW_ITEM__DATA_FETCH_METHOD:
				setDataFetchMethod((BoundaryMethod) newValue);
				return;
			case ClientPackage.TREE_VIEW_ITEM__DROP_METHOD:
				setDropMethod((BoundaryMethod) newValue);
				return;
			case ClientPackage.TREE_VIEW_ITEM__DISPLAY_ATTRIBUTES:
				getDisplayAttributes().clear();
				getDisplayAttributes().addAll((Collection<? extends DTOBeanAttribute>) newValue);
				return;
			case ClientPackage.TREE_VIEW_ITEM__ASSOCIATION:
				setAssociation((AbstractDomainAssociation) newValue);
				return;
			case ClientPackage.TREE_VIEW_ITEM__NODES:
				getNodes().clear();
				getNodes().addAll((Collection<? extends TreeNode>) newValue);
				return;
			case ClientPackage.TREE_VIEW_ITEM__LABEL:
				setLabel((String) newValue);
				return;
			case ClientPackage.TREE_VIEW_ITEM__ITEM_DTO:
				setItemDTO((DTOBean) newValue);
				return;
			case ClientPackage.TREE_VIEW_ITEM__INVISIBLE_ATTRIBUTES:
				getInvisibleAttributes().clear();
				getInvisibleAttributes().addAll((Collection<? extends DTOBeanAttribute>) newValue);
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
			case ClientPackage.TREE_VIEW_ITEM__PARENT_ITEM:
				setParentItem((TreeViewItem) null);
				return;
			case ClientPackage.TREE_VIEW_ITEM__CHILDREN:
				getChildren().clear();
				return;
			case ClientPackage.TREE_VIEW_ITEM__DATA_FETCH_METHOD:
				setDataFetchMethod((BoundaryMethod) null);
				return;
			case ClientPackage.TREE_VIEW_ITEM__DROP_METHOD:
				setDropMethod((BoundaryMethod) null);
				return;
			case ClientPackage.TREE_VIEW_ITEM__DISPLAY_ATTRIBUTES:
				getDisplayAttributes().clear();
				return;
			case ClientPackage.TREE_VIEW_ITEM__ASSOCIATION:
				setAssociation((AbstractDomainAssociation) null);
				return;
			case ClientPackage.TREE_VIEW_ITEM__NODES:
				getNodes().clear();
				return;
			case ClientPackage.TREE_VIEW_ITEM__LABEL:
				setLabel(LABEL_EDEFAULT);
				return;
			case ClientPackage.TREE_VIEW_ITEM__ITEM_DTO:
				setItemDTO((DTOBean) null);
				return;
			case ClientPackage.TREE_VIEW_ITEM__INVISIBLE_ATTRIBUTES:
				getInvisibleAttributes().clear();
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
			case ClientPackage.TREE_VIEW_ITEM__PARENT_ITEM:
				return getParentItem() != null;
			case ClientPackage.TREE_VIEW_ITEM__CHILDREN:
				return children != null && !children.isEmpty();
			case ClientPackage.TREE_VIEW_ITEM__DATA_FETCH_METHOD:
				return dataFetchMethod != null;
			case ClientPackage.TREE_VIEW_ITEM__DROP_METHOD:
				return dropMethod != null;
			case ClientPackage.TREE_VIEW_ITEM__DISPLAY_ATTRIBUTES:
				return displayAttributes != null && !displayAttributes.isEmpty();
			case ClientPackage.TREE_VIEW_ITEM__ASSOCIATION:
				return association != null;
			case ClientPackage.TREE_VIEW_ITEM__NODES:
				return nodes != null && !nodes.isEmpty();
			case ClientPackage.TREE_VIEW_ITEM__LABEL:
				return label != null;
			case ClientPackage.TREE_VIEW_ITEM__ITEM_DTO:
				return itemDTO != null;
			case ClientPackage.TREE_VIEW_ITEM__INVISIBLE_ATTRIBUTES:
				return invisibleAttributes != null && !invisibleAttributes.isEmpty();
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
		result.append(" (label: ");
		result.append(label);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#isRootItem()
	 * @generated not
	 */
	@Override
	public boolean isRootItem() {
		return getParentItem() == null;
	}

}
