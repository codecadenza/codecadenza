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
import net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType;
import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.client.TreeSearchItem;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.client.TreeViewItem;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Tree View</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TreeViewImpl#getRootTreeItem <em>Root Tree Item</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TreeViewImpl#getQuickSearchItems <em>Quick Search Items</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TreeViewImpl#getAdvancedSearchItems <em>Advanced Search Items</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TreeViewImpl#getCountMethod <em>Count Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.client.impl.TreeViewImpl#getRecursiveMethod <em>Recursive Method</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class TreeViewImpl extends FormImpl implements TreeView {
	/**
	 * The cached value of the '{@link #getRootTreeItem() <em>Root Tree Item</em>}' containment reference
	 * @see #getRootTreeItem()
	 * @generated
	 * @ordered
	 */
	protected TreeViewItem rootTreeItem;

	/**
	 * The cached value of the '{@link #getQuickSearchItems() <em>Quick Search Items</em>}' containment reference list
	 * @see #getQuickSearchItems()
	 * @generated
	 * @ordered
	 */
	protected EList<TreeSearchItem> quickSearchItems;

	/**
	 * The cached value of the '{@link #getAdvancedSearchItems() <em>Advanced Search Items</em>}' containment reference list
	 * @see #getAdvancedSearchItems()
	 * @generated
	 * @ordered
	 */
	protected EList<TreeSearchItem> advancedSearchItems;

	/**
	 * The cached value of the '{@link #getCountMethod() <em>Count Method</em>}' reference
	 * @see #getCountMethod()
	 * @generated
	 * @ordered
	 */
	protected BoundaryMethod countMethod;

	/**
	 * The cached value of the '{@link #getRecursiveMethod() <em>Recursive Method</em>}' reference
	 * @see #getRecursiveMethod()
	 * @generated
	 * @ordered
	 */
	protected BoundaryMethod recursiveMethod;

	/**
	 * @generated
	 */
	protected TreeViewImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.impl.FormImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ClientPackage.Literals.TREE_VIEW;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeView#getRootTreeItem()
	 * @generated
	 */
	@Override
	public TreeViewItem getRootTreeItem() {
		return rootTreeItem;
	}

	/**
	 * @param newRootTreeItem
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetRootTreeItem(TreeViewItem newRootTreeItem, NotificationChain msgs) {
		final TreeViewItem oldRootTreeItem = rootTreeItem;
		rootTreeItem = newRootTreeItem;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, ClientPackage.TREE_VIEW__ROOT_TREE_ITEM,
					oldRootTreeItem, newRootTreeItem);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeView#setRootTreeItem(net.codecadenza.eclipse.model.client.TreeViewItem)
	 * @generated
	 */
	@Override
	public void setRootTreeItem(TreeViewItem newRootTreeItem) {
		if (newRootTreeItem != rootTreeItem) {
			NotificationChain msgs = null;

			if (rootTreeItem != null)
				msgs = ((InternalEObject) rootTreeItem).eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE - ClientPackage.TREE_VIEW__ROOT_TREE_ITEM, null, msgs);

			if (newRootTreeItem != null)
				msgs = ((InternalEObject) newRootTreeItem).eInverseAdd(this,
						EOPPOSITE_FEATURE_BASE - ClientPackage.TREE_VIEW__ROOT_TREE_ITEM, null, msgs);

			msgs = basicSetRootTreeItem(newRootTreeItem, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.TREE_VIEW__ROOT_TREE_ITEM, newRootTreeItem,
					newRootTreeItem));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeView#getQuickSearchItems()
	 * @generated
	 */
	@Override
	public EList<TreeSearchItem> getQuickSearchItems() {
		if (quickSearchItems == null)
			quickSearchItems = new EObjectContainmentEList<>(TreeSearchItem.class, this, ClientPackage.TREE_VIEW__QUICK_SEARCH_ITEMS);

		return quickSearchItems;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeView#getAdvancedSearchItems()
	 * @generated
	 */
	@Override
	public EList<TreeSearchItem> getAdvancedSearchItems() {
		if (advancedSearchItems == null)
			advancedSearchItems = new EObjectContainmentEList<>(TreeSearchItem.class, this,
					ClientPackage.TREE_VIEW__ADVANCED_SEARCH_ITEMS);

		return advancedSearchItems;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeView#getCountMethod()
	 * @generated
	 */
	@Override
	public BoundaryMethod getCountMethod() {
		if (countMethod != null && countMethod.eIsProxy()) {
			final var oldCountMethod = (InternalEObject) countMethod;
			countMethod = (BoundaryMethod) eResolveProxy(oldCountMethod);

			if (countMethod != oldCountMethod && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.TREE_VIEW__COUNT_METHOD, oldCountMethod,
						countMethod));
		}

		return countMethod;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public BoundaryMethod basicGetCountMethod() {
		return countMethod;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeView#setCountMethod(net.codecadenza.eclipse.model.boundary.BoundaryMethod)
	 * @generated
	 */
	@Override
	public void setCountMethod(BoundaryMethod newCountMethod) {
		final BoundaryMethod oldCountMethod = countMethod;
		countMethod = newCountMethod;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.TREE_VIEW__COUNT_METHOD, oldCountMethod, countMethod));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeView#getRecursiveMethod()
	 * @generated
	 */
	@Override
	public BoundaryMethod getRecursiveMethod() {
		if (recursiveMethod != null && recursiveMethod.eIsProxy()) {
			final var oldRecursiveMethod = (InternalEObject) recursiveMethod;
			recursiveMethod = (BoundaryMethod) eResolveProxy(oldRecursiveMethod);

			if (recursiveMethod != oldRecursiveMethod && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ClientPackage.TREE_VIEW__RECURSIVE_METHOD, oldRecursiveMethod,
						recursiveMethod));
		}

		return recursiveMethod;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public BoundaryMethod basicGetRecursiveMethod() {
		return recursiveMethod;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeView#setRecursiveMethod(net.codecadenza.eclipse.model.boundary.BoundaryMethod)
	 * @generated
	 */
	@Override
	public void setRecursiveMethod(BoundaryMethod newRecursiveMethod) {
		final BoundaryMethod oldRecursiveMethod = recursiveMethod;
		recursiveMethod = newRecursiveMethod;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ClientPackage.TREE_VIEW__RECURSIVE_METHOD, oldRecursiveMethod,
					recursiveMethod));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.impl.FormImpl#eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ClientPackage.TREE_VIEW__ROOT_TREE_ITEM:
				return basicSetRootTreeItem(null, msgs);
			case ClientPackage.TREE_VIEW__QUICK_SEARCH_ITEMS:
				return ((InternalEList<?>) getQuickSearchItems()).basicRemove(otherEnd, msgs);
			case ClientPackage.TREE_VIEW__ADVANCED_SEARCH_ITEMS:
				return ((InternalEList<?>) getAdvancedSearchItems()).basicRemove(otherEnd, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.impl.FormImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ClientPackage.TREE_VIEW__ROOT_TREE_ITEM:
				return getRootTreeItem();
			case ClientPackage.TREE_VIEW__QUICK_SEARCH_ITEMS:
				return getQuickSearchItems();
			case ClientPackage.TREE_VIEW__ADVANCED_SEARCH_ITEMS:
				return getAdvancedSearchItems();
			case ClientPackage.TREE_VIEW__COUNT_METHOD:
				if (resolve)
					return getCountMethod();

				return basicGetCountMethod();
			case ClientPackage.TREE_VIEW__RECURSIVE_METHOD:
				if (resolve)
					return getRecursiveMethod();

				return basicGetRecursiveMethod();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.impl.FormImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case ClientPackage.TREE_VIEW__ROOT_TREE_ITEM:
				setRootTreeItem((TreeViewItem) newValue);
				return;
			case ClientPackage.TREE_VIEW__QUICK_SEARCH_ITEMS:
				getQuickSearchItems().clear();
				getQuickSearchItems().addAll((Collection<? extends TreeSearchItem>) newValue);
				return;
			case ClientPackage.TREE_VIEW__ADVANCED_SEARCH_ITEMS:
				getAdvancedSearchItems().clear();
				getAdvancedSearchItems().addAll((Collection<? extends TreeSearchItem>) newValue);
				return;
			case ClientPackage.TREE_VIEW__COUNT_METHOD:
				setCountMethod((BoundaryMethod) newValue);
				return;
			case ClientPackage.TREE_VIEW__RECURSIVE_METHOD:
				setRecursiveMethod((BoundaryMethod) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.impl.FormImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case ClientPackage.TREE_VIEW__ROOT_TREE_ITEM:
				setRootTreeItem((TreeViewItem) null);
				return;
			case ClientPackage.TREE_VIEW__QUICK_SEARCH_ITEMS:
				getQuickSearchItems().clear();
				return;
			case ClientPackage.TREE_VIEW__ADVANCED_SEARCH_ITEMS:
				getAdvancedSearchItems().clear();
				return;
			case ClientPackage.TREE_VIEW__COUNT_METHOD:
				setCountMethod((BoundaryMethod) null);
				return;
			case ClientPackage.TREE_VIEW__RECURSIVE_METHOD:
				setRecursiveMethod((BoundaryMethod) null);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.impl.FormImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case ClientPackage.TREE_VIEW__ROOT_TREE_ITEM:
				return rootTreeItem != null;
			case ClientPackage.TREE_VIEW__QUICK_SEARCH_ITEMS:
				return quickSearchItems != null && !quickSearchItems.isEmpty();
			case ClientPackage.TREE_VIEW__ADVANCED_SEARCH_ITEMS:
				return advancedSearchItems != null && !advancedSearchItems.isEmpty();
			case ClientPackage.TREE_VIEW__COUNT_METHOD:
				return countMethod != null;
			case ClientPackage.TREE_VIEW__RECURSIVE_METHOD:
				return recursiveMethod != null;
		}

		return super.eIsSet(featureID);
	}

	/**
	 * @param parentItem
	 * @return a list containing all sub-items of a given tree view item
	 * @generated not
	 */
	private EList<TreeViewItem> getAllSubTreeItems(TreeViewItem parentItem) {
		final var items = new BasicEList<TreeViewItem>();
		items.add(parentItem);

		parentItem.getChildren().forEach(item -> items.addAll(getAllSubTreeItems(item)));

		return items;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeView#getAllSubTreeItems()
	 * @generated not
	 */
	@Override
	public EList<TreeViewItem> getAllSubTreeItems() {
		final var items = new BasicEList<TreeViewItem>();

		getRootTreeItem().getChildren().forEach(item -> items.addAll(getAllSubTreeItems(item)));

		return items;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.TreeView#needsSearchObject()
	 * @generated not
	 */
	@Override
	public boolean needsSearchObject() {
		if (!getAdvancedSearchItems().isEmpty() || !getQuickSearchItems().isEmpty())
			return true;

		return getRecursiveMethod() != null || getBoundaryMethod().getDataFetchType() != BoundaryMethodDataFetchType.DEFAULT;
	}

}
