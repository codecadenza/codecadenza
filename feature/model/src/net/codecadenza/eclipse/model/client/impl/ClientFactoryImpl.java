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

import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.ClientFactory;
import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.TreeNode;
import net.codecadenza.eclipse.model.client.TreeSearchItem;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.client.TreeViewItem;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EFactoryImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * An implementation of the model factory.
 * @generated
 */
public class ClientFactoryImpl extends EFactoryImpl implements ClientFactory {
	/**
	 * @return the default factory implementation
	 * @generated
	 */
	public static ClientFactory init() {
		try {
			final var theClientFactory = (ClientFactory) EPackage.Registry.INSTANCE.getEFactory(ClientPackage.eNS_URI);

			if (theClientFactory != null)
				return theClientFactory;
		}
		catch (final Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}

		return new ClientFactoryImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#create(org.eclipse.emf.ecore.EClass)
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		return switch (eClass.getClassifierID()) {
			case ClientPackage.FORM -> createForm();
			case ClientPackage.FORM_ACTION -> createFormAction();
			case ClientPackage.FORM_FIELD -> createFormField();
			case ClientPackage.FORM_GROUP -> createFormGroup();
			case ClientPackage.FORM_PANEL -> createFormPanel();
			case ClientPackage.FORM_TABLE -> createFormTable();
			case ClientPackage.TABLE_COLUMN_FIELD -> createTableColumnField();
			case ClientPackage.TREE_VIEW -> createTreeView();
			case ClientPackage.TREE_VIEW_ITEM -> createTreeViewItem();
			case ClientPackage.TREE_SEARCH_ITEM -> createTreeSearchItem();
			case ClientPackage.TREE_NODE -> createTreeNode();
			default -> throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#createFromString(org.eclipse.emf.ecore.EDataType, java.lang.String)
	 * @generated
	 */
	@Override
	public Object createFromString(EDataType eDataType, String initialValue) {
		return switch (eDataType.getClassifierID()) {
			case ClientPackage.FORM_FIELD_TYPE_ENUMERATION -> createFormFieldTypeEnumerationFromString(eDataType, initialValue);
			case ClientPackage.FORM_TYPE_ENUMERATION -> createFormTypeEnumerationFromString(eDataType, initialValue);
			case ClientPackage.TABLE_COLUMN_FIELD_TYPE_ENUMERATION -> createTableColumnFieldTypeEnumerationFromString(eDataType,
					initialValue);
			case ClientPackage.ACTION_TYPE -> createActionTypeFromString(eDataType, initialValue);
			default -> throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#convertToString(org.eclipse.emf.ecore.EDataType, java.lang.Object)
	 * @generated
	 */
	@Override
	public String convertToString(EDataType eDataType, Object instanceValue) {
		return switch (eDataType.getClassifierID()) {
			case ClientPackage.FORM_FIELD_TYPE_ENUMERATION -> convertFormFieldTypeEnumerationToString(eDataType, instanceValue);
			case ClientPackage.FORM_TYPE_ENUMERATION -> convertFormTypeEnumerationToString(eDataType, instanceValue);
			case ClientPackage.TABLE_COLUMN_FIELD_TYPE_ENUMERATION -> convertTableColumnFieldTypeEnumerationToString(eDataType,
					instanceValue);
			case ClientPackage.ACTION_TYPE -> convertActionTypeToString(eDataType, instanceValue);
			default -> throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientFactory#createForm()
	 * @generated
	 */
	@Override
	public Form createForm() {
		return new FormImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientFactory#createFormAction()
	 * @generated
	 */
	@Override
	public FormAction createFormAction() {
		return new FormActionImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientFactory#createFormField()
	 * @generated
	 */
	@Override
	public FormField createFormField() {
		return new FormFieldImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientFactory#createFormGroup()
	 * @generated
	 */
	@Override
	public FormGroup createFormGroup() {
		return new FormGroupImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientFactory#createFormPanel()
	 * @generated
	 */
	@Override
	public FormPanel createFormPanel() {
		return new FormPanelImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientFactory#createFormTable()
	 * @generated
	 */
	@Override
	public FormTable createFormTable() {
		return new FormTableImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientFactory#createTableColumnField()
	 * @generated
	 */
	@Override
	public TableColumnField createTableColumnField() {
		return new TableColumnFieldImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientFactory#createTreeView()
	 * @generated
	 */
	@Override
	public TreeView createTreeView() {
		return new TreeViewImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientFactory#createTreeViewItem()
	 * @generated
	 */
	@Override
	public TreeViewItem createTreeViewItem() {
		return new TreeViewItemImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientFactory#createTreeSearchItem()
	 * @generated
	 */
	@Override
	public TreeSearchItem createTreeSearchItem() {
		return new TreeSearchItemImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientFactory#createTreeNode()
	 * @generated
	 */
	@Override
	public TreeNode createTreeNode() {
		return new TreeNodeImpl();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public FormFieldTypeEnumeration createFormFieldTypeEnumerationFromString(EDataType eDataType, String initialValue) {
		final FormFieldTypeEnumeration result = FormFieldTypeEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertFormFieldTypeEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public FormTypeEnumeration createFormTypeEnumerationFromString(EDataType eDataType, String initialValue) {
		final FormTypeEnumeration result = FormTypeEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertFormTypeEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public TableColumnFieldTypeEnumeration createTableColumnFieldTypeEnumerationFromString(EDataType eDataType,
			String initialValue) {
		final TableColumnFieldTypeEnumeration result = TableColumnFieldTypeEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertTableColumnFieldTypeEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public ActionType createActionTypeFromString(EDataType eDataType, String initialValue) {
		final ActionType result = ActionType.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertActionTypeToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientFactory#getClientPackage()
	 * @generated
	 */
	@Override
	public ClientPackage getClientPackage() {
		return (ClientPackage) getEPackage();
	}

	/**
	 * @deprecated
	 * @return the client package
	 * @generated
	 */
	@Deprecated
	public static ClientPackage getPackage() {
		return ClientPackage.eINSTANCE;
	}

}
