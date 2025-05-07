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
package net.codecadenza.eclipse.model.client;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * The <b>Package</b> for the model. It contains accessors for the meta objects to represent
 * <ul>
 * <li>each class,</li>
 * <li>each feature of each class,</li>
 * <li>each enum,</li>
 * <li>and each data type</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.client.ClientFactory
 * @model kind="package"
 * @generated
 */
public interface ClientPackage extends EPackage {
	/**
	 * The package name
	 * @generated
	 */
	String eNAME = "client";

	/**
	 * The package namespace URI
	 * @generated
	 */
	String eNS_URI = "http:///net/codecadenza/eclipse/model/client.ecore";

	/**
	 * The package namespace name
	 * @generated
	 */
	String eNS_PREFIX = "net.codecadenza.eclipse.model.client";

	/**
	 * The singleton instance of the package
	 * @generated
	 */
	ClientPackage eINSTANCE = net.codecadenza.eclipse.model.client.impl.ClientPackageImpl.init();

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.client.impl.FormImpl <em>Form</em>}' class
	 * @see net.codecadenza.eclipse.model.client.impl.FormImpl
	 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getForm()
	 * @generated
	 */
	int FORM = 0;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Form Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM__FORM_TYPE = 1;

	/**
	 * The feature ID for the '<em><b>Form Group</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM__FORM_GROUP = 2;

	/**
	 * The feature ID for the '<em><b>Title</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM__TITLE = 3;

	/**
	 * The feature ID for the '<em><b>Modal</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM__MODAL = 4;

	/**
	 * The feature ID for the '<em><b>Resizable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM__RESIZABLE = 5;

	/**
	 * The feature ID for the '<em><b>Title Area</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM__TITLE_AREA = 6;

	/**
	 * The feature ID for the '<em><b>Open Edit After Create</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM__OPEN_EDIT_AFTER_CREATE = 7;

	/**
	 * The feature ID for the '<em><b>Height</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM__HEIGHT = 8;

	/**
	 * The feature ID for the '<em><b>Width</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM__WIDTH = 9;

	/**
	 * The feature ID for the '<em><b>Form Panels</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int FORM__FORM_PANELS = 10;

	/**
	 * The feature ID for the '<em><b>Actions</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int FORM__ACTIONS = 11;

	/**
	 * The feature ID for the '<em><b>DTO</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM__DTO = 12;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM__DOMAIN_OBJECT = 13;

	/**
	 * The feature ID for the '<em><b>Roles</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int FORM__ROLES = 14;

	/**
	 * The feature ID for the '<em><b>Boundary Method</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM__BOUNDARY_METHOD = 15;

	/**
	 * The number of structural features of the '<em>Form</em>' class
	 * @generated
	 * @ordered
	 */
	int FORM_FEATURE_COUNT = 16;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.client.impl.FormActionImpl <em>Form Action</em>}' class
	 * @see net.codecadenza.eclipse.model.client.impl.FormActionImpl
	 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getFormAction()
	 * @generated
	 */
	int FORM_ACTION = 1;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_ACTION__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Description</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_ACTION__DESCRIPTION = 1;

	/**
	 * The feature ID for the '<em><b>Form</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int FORM_ACTION__FORM = 2;

	/**
	 * The feature ID for the '<em><b>Panel</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int FORM_ACTION__PANEL = 3;

	/**
	 * The feature ID for the '<em><b>Target Form</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM_ACTION__TARGET_FORM = 4;

	/**
	 * The feature ID for the '<em><b>Boundary Method</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM_ACTION__BOUNDARY_METHOD = 5;

	/**
	 * The feature ID for the '<em><b>Roles</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int FORM_ACTION__ROLES = 6;

	/**
	 * The feature ID for the '<em><b>Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_ACTION__TYPE = 7;

	/**
	 * The feature ID for the '<em><b>Label</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_ACTION__LABEL = 8;

	/**
	 * The number of structural features of the '<em>Form Action</em>' class
	 * @generated
	 * @ordered
	 */
	int FORM_ACTION_FEATURE_COUNT = 9;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.client.impl.FormFieldImpl <em>Form Field</em>}' class
	 * @see net.codecadenza.eclipse.model.client.impl.FormFieldImpl
	 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getFormField()
	 * @generated
	 */
	int FORM_FIELD = 2;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_FIELD__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Col Index</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_FIELD__COL_INDEX = 1;

	/**
	 * The feature ID for the '<em><b>Row Index</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_FIELD__ROW_INDEX = 2;

	/**
	 * The feature ID for the '<em><b>Visible</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_FIELD__VISIBLE = 3;

	/**
	 * The feature ID for the '<em><b>Label</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_FIELD__LABEL = 4;

	/**
	 * The feature ID for the '<em><b>Span Cols</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_FIELD__SPAN_COLS = 5;

	/**
	 * The feature ID for the '<em><b>Readonly</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_FIELD__READONLY = 6;

	/**
	 * The feature ID for the '<em><b>Mandatory</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_FIELD__MANDATORY = 7;

	/**
	 * The feature ID for the '<em><b>Field Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_FIELD__FIELD_TYPE = 8;

	/**
	 * The feature ID for the '<em><b>Panel</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int FORM_FIELD__PANEL = 9;

	/**
	 * The feature ID for the '<em><b>Width</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_FIELD__WIDTH = 10;

	/**
	 * The feature ID for the '<em><b>Default Value</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_FIELD__DEFAULT_VALUE = 11;

	/**
	 * The feature ID for the '<em><b>List Of Values</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM_FIELD__LIST_OF_VALUES = 12;

	/**
	 * The feature ID for the '<em><b>DTO Attribute</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM_FIELD__DTO_ATTRIBUTE = 13;

	/**
	 * The feature ID for the '<em><b>Add Form Link To Label</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_FIELD__ADD_FORM_LINK_TO_LABEL = 14;

	/**
	 * The number of structural features of the '<em>Form Field</em>' class
	 * @generated
	 * @ordered
	 */
	int FORM_FIELD_FEATURE_COUNT = 15;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.client.impl.FormGroupImpl <em>Form Group</em>}' class
	 * @see net.codecadenza.eclipse.model.client.impl.FormGroupImpl
	 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getFormGroup()
	 * @generated
	 */
	int FORM_GROUP = 3;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_GROUP__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Group Order</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_GROUP__GROUP_ORDER = 1;

	/**
	 * The feature ID for the '<em><b>Parent Group</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM_GROUP__PARENT_GROUP = 2;

	/**
	 * The feature ID for the '<em><b>Child Groups</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int FORM_GROUP__CHILD_GROUPS = 3;

	/**
	 * The feature ID for the '<em><b>Forms</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int FORM_GROUP__FORMS = 4;

	/**
	 * The feature ID for the '<em><b>Panels</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int FORM_GROUP__PANELS = 5;

	/**
	 * The feature ID for the '<em><b>Roles</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int FORM_GROUP__ROLES = 6;

	/**
	 * The feature ID for the '<em><b>Project</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM_GROUP__PROJECT = 7;

	/**
	 * The number of structural features of the '<em>Form Group</em>' class
	 * @generated
	 * @ordered
	 */
	int FORM_GROUP_FEATURE_COUNT = 8;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl <em>Form Panel</em>}' class
	 * @see net.codecadenza.eclipse.model.client.impl.FormPanelImpl
	 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getFormPanel()
	 * @generated
	 */
	int FORM_PANEL = 4;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Column Count</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL__COLUMN_COUNT = 1;

	/**
	 * The feature ID for the '<em><b>Col Index</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL__COL_INDEX = 2;

	/**
	 * The feature ID for the '<em><b>Row Index</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL__ROW_INDEX = 3;

	/**
	 * The feature ID for the '<em><b>Verticalspan</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL__VERTICALSPAN = 4;

	/**
	 * The feature ID for the '<em><b>Horizontal Span</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL__HORIZONTAL_SPAN = 5;

	/**
	 * The feature ID for the '<em><b>Form Group</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL__FORM_GROUP = 6;

	/**
	 * The feature ID for the '<em><b>Label</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL__LABEL = 7;

	/**
	 * The feature ID for the '<em><b>Form</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL__FORM = 8;

	/**
	 * The feature ID for the '<em><b>Draw Border</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL__DRAW_BORDER = 9;

	/**
	 * The feature ID for the '<em><b>Base Panel</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL__BASE_PANEL = 10;

	/**
	 * The feature ID for the '<em><b>Form Table</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL__FORM_TABLE = 11;

	/**
	 * The feature ID for the '<em><b>Fields</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL__FIELDS = 12;

	/**
	 * The feature ID for the '<em><b>Actions</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL__ACTIONS = 13;

	/**
	 * The feature ID for the '<em><b>DTO</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL__DTO = 14;

	/**
	 * The feature ID for the '<em><b>Boundary Method</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL__BOUNDARY_METHOD = 15;

	/**
	 * The feature ID for the '<em><b>Association</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL__ASSOCIATION = 16;

	/**
	 * The number of structural features of the '<em>Form Panel</em>' class
	 * @generated
	 * @ordered
	 */
	int FORM_PANEL_FEATURE_COUNT = 17;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.client.impl.FormTableImpl <em>Form Table</em>}' class
	 * @see net.codecadenza.eclipse.model.client.impl.FormTableImpl
	 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getFormTable()
	 * @generated
	 */
	int FORM_TABLE = 5;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_TABLE__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Col Index</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_TABLE__COL_INDEX = 1;

	/**
	 * The feature ID for the '<em><b>Row Index</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_TABLE__ROW_INDEX = 2;

	/**
	 * The feature ID for the '<em><b>Span Cols</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_TABLE__SPAN_COLS = 3;

	/**
	 * The feature ID for the '<em><b>Span Rows</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_TABLE__SPAN_ROWS = 4;

	/**
	 * The feature ID for the '<em><b>Vertical Span</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_TABLE__VERTICAL_SPAN = 5;

	/**
	 * The feature ID for the '<em><b>Horizontal Span</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FORM_TABLE__HORIZONTAL_SPAN = 6;

	/**
	 * The feature ID for the '<em><b>Form Panel</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM_TABLE__FORM_PANEL = 7;

	/**
	 * The feature ID for the '<em><b>Fields</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int FORM_TABLE__FIELDS = 8;

	/**
	 * The feature ID for the '<em><b>Association</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FORM_TABLE__ASSOCIATION = 9;

	/**
	 * The number of structural features of the '<em>Form Table</em>' class
	 * @generated
	 * @ordered
	 */
	int FORM_TABLE_FEATURE_COUNT = 10;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.client.impl.TableColumnFieldImpl <em>Table Column
	 * Field</em>}' class
	 * @see net.codecadenza.eclipse.model.client.impl.TableColumnFieldImpl
	 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getTableColumnField()
	 * @generated
	 */
	int TABLE_COLUMN_FIELD = 6;

	/**
	 * The feature ID for the '<em><b>Col Index</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TABLE_COLUMN_FIELD__COL_INDEX = 0;

	/**
	 * The feature ID for the '<em><b>Visible</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TABLE_COLUMN_FIELD__VISIBLE = 1;

	/**
	 * The feature ID for the '<em><b>Identifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TABLE_COLUMN_FIELD__IDENTIFIER = 2;

	/**
	 * The feature ID for the '<em><b>Form Table</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int TABLE_COLUMN_FIELD__FORM_TABLE = 3;

	/**
	 * The feature ID for the '<em><b>Width</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TABLE_COLUMN_FIELD__WIDTH = 4;

	/**
	 * The feature ID for the '<em><b>Title</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TABLE_COLUMN_FIELD__TITLE = 5;

	/**
	 * The feature ID for the '<em><b>Lov Form</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TABLE_COLUMN_FIELD__LOV_FORM = 6;

	/**
	 * The feature ID for the '<em><b>Field Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TABLE_COLUMN_FIELD__FIELD_TYPE = 7;

	/**
	 * The feature ID for the '<em><b>Association Ref</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TABLE_COLUMN_FIELD__ASSOCIATION_REF = 8;

	/**
	 * The feature ID for the '<em><b>Searchable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TABLE_COLUMN_FIELD__SEARCHABLE = 9;

	/**
	 * The feature ID for the '<em><b>DTO Attribute</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TABLE_COLUMN_FIELD__DTO_ATTRIBUTE = 10;

	/**
	 * The number of structural features of the '<em>Table Column Field</em>' class
	 * @generated
	 * @ordered
	 */
	int TABLE_COLUMN_FIELD_FEATURE_COUNT = 11;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.client.impl.TreeViewImpl <em>Tree View</em>}' class
	 * @see net.codecadenza.eclipse.model.client.impl.TreeViewImpl
	 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getTreeView()
	 * @generated
	 */
	int TREE_VIEW = 7;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__NAME = FORM__NAME;

	/**
	 * The feature ID for the '<em><b>Form Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__FORM_TYPE = FORM__FORM_TYPE;

	/**
	 * The feature ID for the '<em><b>Form Group</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__FORM_GROUP = FORM__FORM_GROUP;

	/**
	 * The feature ID for the '<em><b>Title</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__TITLE = FORM__TITLE;

	/**
	 * The feature ID for the '<em><b>Modal</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__MODAL = FORM__MODAL;

	/**
	 * The feature ID for the '<em><b>Resizable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__RESIZABLE = FORM__RESIZABLE;

	/**
	 * The feature ID for the '<em><b>Title Area</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__TITLE_AREA = FORM__TITLE_AREA;

	/**
	 * The feature ID for the '<em><b>Open Edit After Create</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__OPEN_EDIT_AFTER_CREATE = FORM__OPEN_EDIT_AFTER_CREATE;

	/**
	 * The feature ID for the '<em><b>Height</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__HEIGHT = FORM__HEIGHT;

	/**
	 * The feature ID for the '<em><b>Width</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__WIDTH = FORM__WIDTH;

	/**
	 * The feature ID for the '<em><b>Form Panels</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__FORM_PANELS = FORM__FORM_PANELS;

	/**
	 * The feature ID for the '<em><b>Actions</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__ACTIONS = FORM__ACTIONS;

	/**
	 * The feature ID for the '<em><b>DTO</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__DTO = FORM__DTO;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__DOMAIN_OBJECT = FORM__DOMAIN_OBJECT;

	/**
	 * The feature ID for the '<em><b>Roles</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__ROLES = FORM__ROLES;

	/**
	 * The feature ID for the '<em><b>Boundary Method</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__BOUNDARY_METHOD = FORM__BOUNDARY_METHOD;

	/**
	 * The feature ID for the '<em><b>Root Tree Item</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__ROOT_TREE_ITEM = FORM_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Quick Search Items</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__QUICK_SEARCH_ITEMS = FORM_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Advanced Search Items</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__ADVANCED_SEARCH_ITEMS = FORM_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>Count Method</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__COUNT_METHOD = FORM_FEATURE_COUNT + 3;

	/**
	 * The feature ID for the '<em><b>Recursive Method</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW__RECURSIVE_METHOD = FORM_FEATURE_COUNT + 4;

	/**
	 * The number of structural features of the '<em>Tree View</em>' class
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW_FEATURE_COUNT = FORM_FEATURE_COUNT + 5;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.client.impl.TreeViewItemImpl <em>Tree View Item</em>}' class
	 * @see net.codecadenza.eclipse.model.client.impl.TreeViewItemImpl
	 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getTreeViewItem()
	 * @generated
	 */
	int TREE_VIEW_ITEM = 8;

	/**
	 * The feature ID for the '<em><b>Parent Item</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW_ITEM__PARENT_ITEM = 0;

	/**
	 * The feature ID for the '<em><b>Children</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW_ITEM__CHILDREN = 1;

	/**
	 * The feature ID for the '<em><b>Data Fetch Method</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW_ITEM__DATA_FETCH_METHOD = 2;

	/**
	 * The feature ID for the '<em><b>Drop Method</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW_ITEM__DROP_METHOD = 3;

	/**
	 * The feature ID for the '<em><b>Display Attributes</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW_ITEM__DISPLAY_ATTRIBUTES = 4;

	/**
	 * The feature ID for the '<em><b>Association</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW_ITEM__ASSOCIATION = 5;

	/**
	 * The feature ID for the '<em><b>Nodes</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW_ITEM__NODES = 6;

	/**
	 * The feature ID for the '<em><b>Label</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW_ITEM__LABEL = 7;

	/**
	 * The feature ID for the '<em><b>Item DTO</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW_ITEM__ITEM_DTO = 8;

	/**
	 * The feature ID for the '<em><b>Invisible Attributes</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW_ITEM__INVISIBLE_ATTRIBUTES = 9;

	/**
	 * The number of structural features of the '<em>Tree View Item</em>' class
	 * @generated
	 * @ordered
	 */
	int TREE_VIEW_ITEM_FEATURE_COUNT = 10;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.client.impl.TreeSearchItemImpl <em>Tree Search Item</em>}'
	 * class
	 * @see net.codecadenza.eclipse.model.client.impl.TreeSearchItemImpl
	 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getTreeSearchItem()
	 * @generated
	 */
	int TREE_SEARCH_ITEM = 9;

	/**
	 * The feature ID for the '<em><b>Label</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TREE_SEARCH_ITEM__LABEL = 0;

	/**
	 * The feature ID for the '<em><b>DTO Attribute</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TREE_SEARCH_ITEM__DTO_ATTRIBUTE = 1;

	/**
	 * The number of structural features of the '<em>Tree Search Item</em>' class
	 * @generated
	 * @ordered
	 */
	int TREE_SEARCH_ITEM_FEATURE_COUNT = 2;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.client.impl.TreeNodeImpl <em>Tree Node</em>}' class
	 * @see net.codecadenza.eclipse.model.client.impl.TreeNodeImpl
	 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getTreeNode()
	 * @generated
	 */
	int TREE_NODE = 10;

	/**
	 * The feature ID for the '<em><b>Label</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int TREE_NODE__LABEL = 0;

	/**
	 * The feature ID for the '<em><b>DTO Attribute</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int TREE_NODE__DTO_ATTRIBUTE = 1;

	/**
	 * The number of structural features of the '<em>Tree Node</em>' class
	 * @generated
	 * @ordered
	 */
	int TREE_NODE_FEATURE_COUNT = 2;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration <em>Form Field Type
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration
	 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getFormFieldTypeEnumeration()
	 * @generated
	 */
	int FORM_FIELD_TYPE_ENUMERATION = 11;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.client.FormTypeEnumeration <em>Form Type Enumeration</em>}'
	 * enum
	 * @see net.codecadenza.eclipse.model.client.FormTypeEnumeration
	 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getFormTypeEnumeration()
	 * @generated
	 */
	int FORM_TYPE_ENUMERATION = 12;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration <em>Table Column
	 * Field Type Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration
	 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getTableColumnFieldTypeEnumeration()
	 * @generated
	 */
	int TABLE_COLUMN_FIELD_TYPE_ENUMERATION = 13;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.client.ActionType <em>Action Type</em>}' enum
	 * @see net.codecadenza.eclipse.model.client.ActionType
	 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getActionType()
	 * @generated
	 */
	int ACTION_TYPE = 14;

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.client.Form <em>Form</em>}'
	 * @return the meta object for class '<em>Form</em>'
	 * @see net.codecadenza.eclipse.model.client.Form
	 * @generated
	 */
	EClass getForm();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.Form#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.client.Form#getName()
	 * @see #getForm()
	 * @generated
	 */
	EAttribute getForm_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.Form#getFormType <em>Form Type</em>}'
	 * @return the meta object for the attribute '<em>Form Type</em>'
	 * @see net.codecadenza.eclipse.model.client.Form#getFormType()
	 * @see #getForm()
	 * @generated
	 */
	EAttribute getForm_FormType();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.Form#getFormGroup <em>Form Group</em>}'
	 * @return the meta object for the reference '<em>Form Group</em>'
	 * @see net.codecadenza.eclipse.model.client.Form#getFormGroup()
	 * @see #getForm()
	 * @generated
	 */
	EReference getForm_FormGroup();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.Form#getTitle <em>Title</em>}'
	 * @return the meta object for the attribute '<em>Title</em>'
	 * @see net.codecadenza.eclipse.model.client.Form#getTitle()
	 * @see #getForm()
	 * @generated
	 */
	EAttribute getForm_Title();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.Form#isModal <em>Modal</em>}'
	 * @return the meta object for the attribute '<em>Modal</em>'
	 * @see net.codecadenza.eclipse.model.client.Form#isModal()
	 * @see #getForm()
	 * @generated
	 */
	EAttribute getForm_Modal();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.Form#isResizable <em>Resizable</em>}'
	 * @return the meta object for the attribute '<em>Resizable</em>'
	 * @see net.codecadenza.eclipse.model.client.Form#isResizable()
	 * @see #getForm()
	 * @generated
	 */
	EAttribute getForm_Resizable();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.Form#isTitleArea <em>Title Area</em>}'
	 * @return the meta object for the attribute '<em>Title Area</em>'
	 * @see net.codecadenza.eclipse.model.client.Form#isTitleArea()
	 * @see #getForm()
	 * @generated
	 */
	EAttribute getForm_TitleArea();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.Form#isOpenEditAfterCreate <em>Open
	 * Edit After Create</em>}'
	 * @return the meta object for the attribute '<em>Open Edit After Create</em>'
	 * @see net.codecadenza.eclipse.model.client.Form#isOpenEditAfterCreate()
	 * @see #getForm()
	 * @generated
	 */
	EAttribute getForm_OpenEditAfterCreate();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.Form#getHeight <em>Height</em>}'
	 * @return the meta object for the attribute '<em>Height</em>'
	 * @see net.codecadenza.eclipse.model.client.Form#getHeight()
	 * @see #getForm()
	 * @generated
	 */
	EAttribute getForm_Height();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.Form#getWidth <em>Width</em>}'
	 * @return the meta object for the attribute '<em>Width</em>'
	 * @see net.codecadenza.eclipse.model.client.Form#getWidth()
	 * @see #getForm()
	 * @generated
	 */
	EAttribute getForm_Width();

	/**
	 * Return the meta object for the reference list '{@link net.codecadenza.eclipse.model.client.Form#getFormPanels <em>Form
	 * Panels</em>}'
	 * @return the meta object for the reference list '<em>Form Panels</em>'
	 * @see net.codecadenza.eclipse.model.client.Form#getFormPanels()
	 * @see #getForm()
	 * @generated
	 */
	EReference getForm_FormPanels();

	/**
	 * Return the meta object for the containment reference list '{@link net.codecadenza.eclipse.model.client.Form#getActions
	 * <em>Actions</em>}'
	 * @return the meta object for the containment reference list '<em>Actions</em>'
	 * @see net.codecadenza.eclipse.model.client.Form#getActions()
	 * @see #getForm()
	 * @generated
	 */
	EReference getForm_Actions();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.Form#getDTO <em>DTO</em>}'
	 * @return the meta object for the reference '<em>DTO</em>'
	 * @see net.codecadenza.eclipse.model.client.Form#getDTO()
	 * @see #getForm()
	 * @generated
	 */
	EReference getForm_DTO();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.Form#getDomainObject <em>Domain
	 * Object</em>}'
	 * @return the meta object for the reference '<em>Domain Object</em>'
	 * @see net.codecadenza.eclipse.model.client.Form#getDomainObject()
	 * @see #getForm()
	 * @generated
	 */
	EReference getForm_DomainObject();

	/**
	 * Return the meta object for the reference list '{@link net.codecadenza.eclipse.model.client.Form#getRoles <em>Roles</em>}'
	 * @return the meta object for the reference list '<em>Roles</em>'
	 * @see net.codecadenza.eclipse.model.client.Form#getRoles()
	 * @see #getForm()
	 * @generated
	 */
	EReference getForm_Roles();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.Form#getBoundaryMethod <em>Boundary
	 * Method</em>}'
	 * @return the meta object for the reference '<em>Boundary Method</em>'
	 * @see net.codecadenza.eclipse.model.client.Form#getBoundaryMethod()
	 * @see #getForm()
	 * @generated
	 */
	EReference getForm_BoundaryMethod();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.client.FormAction <em>Form Action</em>}'
	 * @return the meta object for class '<em>Form Action</em>'
	 * @see net.codecadenza.eclipse.model.client.FormAction
	 * @generated
	 */
	EClass getFormAction();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormAction#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.client.FormAction#getName()
	 * @see #getFormAction()
	 * @generated
	 */
	EAttribute getFormAction_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormAction#getDescription
	 * <em>Description</em>}'
	 * @return the meta object for the attribute '<em>Description</em>'
	 * @see net.codecadenza.eclipse.model.client.FormAction#getDescription()
	 * @see #getFormAction()
	 * @generated
	 */
	EAttribute getFormAction_Description();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.client.FormAction#getForm
	 * <em>Form</em>}'
	 * @return the meta object for the container reference '<em>Form</em>'
	 * @see net.codecadenza.eclipse.model.client.FormAction#getForm()
	 * @see #getFormAction()
	 * @generated
	 */
	EReference getFormAction_Form();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.client.FormAction#getPanel
	 * <em>Panel</em>}'
	 * @return the meta object for the container reference '<em>Panel</em>'
	 * @see net.codecadenza.eclipse.model.client.FormAction#getPanel()
	 * @see #getFormAction()
	 * @generated
	 */
	EReference getFormAction_Panel();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.FormAction#getTargetForm <em>Target
	 * Form</em>}'
	 * @return the meta object for the reference '<em>Target Form</em>'
	 * @see net.codecadenza.eclipse.model.client.FormAction#getTargetForm()
	 * @see #getFormAction()
	 * @generated
	 */
	EReference getFormAction_TargetForm();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.FormAction#getBoundaryMethod
	 * <em>Boundary Method</em>}'
	 * @return the meta object for the reference '<em>Boundary Method</em>'
	 * @see net.codecadenza.eclipse.model.client.FormAction#getBoundaryMethod()
	 * @see #getFormAction()
	 * @generated
	 */
	EReference getFormAction_BoundaryMethod();

	/**
	 * Return the meta object for the reference list '{@link net.codecadenza.eclipse.model.client.FormAction#getRoles
	 * <em>Roles</em>}'
	 * @return the meta object for the reference list '<em>Roles</em>'
	 * @see net.codecadenza.eclipse.model.client.FormAction#getRoles()
	 * @see #getFormAction()
	 * @generated
	 */
	EReference getFormAction_Roles();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormAction#getType <em>Action
	 * Type</em>}'
	 * @return the meta object for the attribute '<em>Action Type</em>'
	 * @see net.codecadenza.eclipse.model.client.FormAction#getType()
	 * @see #getFormAction()
	 * @generated
	 */
	EAttribute getFormAction_Type();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormAction#getLabel <em>Label</em>}'
	 * @return the meta object for the attribute '<em>Label</em>'
	 * @see net.codecadenza.eclipse.model.client.FormAction#getLabel()
	 * @see #getFormAction()
	 * @generated
	 */
	EAttribute getFormAction_Label();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.client.FormField <em>Form Field</em>}'
	 * @return the meta object for class '<em>Form Field</em>'
	 * @see net.codecadenza.eclipse.model.client.FormField
	 * @generated
	 */
	EClass getFormField();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormField#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.client.FormField#getName()
	 * @see #getFormField()
	 * @generated
	 */
	EAttribute getFormField_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormField#getColIndex <em>Col
	 * Index</em>}'
	 * @return the meta object for the attribute '<em>Col Index</em>'
	 * @see net.codecadenza.eclipse.model.client.FormField#getColIndex()
	 * @see #getFormField()
	 * @generated
	 */
	EAttribute getFormField_ColIndex();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormField#getRowIndex <em>Row
	 * Index</em>}'
	 * @return the meta object for the attribute '<em>Row Index</em>'
	 * @see net.codecadenza.eclipse.model.client.FormField#getRowIndex()
	 * @see #getFormField()
	 * @generated
	 */
	EAttribute getFormField_RowIndex();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormField#isVisible <em>Visible</em>}'
	 * @return the meta object for the attribute '<em>Visible</em>'
	 * @see net.codecadenza.eclipse.model.client.FormField#isVisible()
	 * @see #getFormField()
	 * @generated
	 */
	EAttribute getFormField_Visible();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormField#getLabel <em>Label</em>}'
	 * @return the meta object for the attribute '<em>Label</em>'
	 * @see net.codecadenza.eclipse.model.client.FormField#getLabel()
	 * @see #getFormField()
	 * @generated
	 */
	EAttribute getFormField_Label();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormField#isSpanCols <em>Span
	 * Cols</em>}'
	 * @return the meta object for the attribute '<em>Span Cols</em>'
	 * @see net.codecadenza.eclipse.model.client.FormField#isSpanCols()
	 * @see #getFormField()
	 * @generated
	 */
	EAttribute getFormField_SpanCols();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormField#isReadonly
	 * <em>Readonly</em>}'
	 * @return the meta object for the attribute '<em>Readonly</em>'
	 * @see net.codecadenza.eclipse.model.client.FormField#isReadonly()
	 * @see #getFormField()
	 * @generated
	 */
	EAttribute getFormField_Readonly();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormField#isMandatory
	 * <em>Mandatory</em>}'
	 * @return the meta object for the attribute '<em>Mandatory</em>'
	 * @see net.codecadenza.eclipse.model.client.FormField#isMandatory()
	 * @see #getFormField()
	 * @generated
	 */
	EAttribute getFormField_Mandatory();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormField#getFieldType <em>Field
	 * Type</em>}'
	 * @return the meta object for the attribute '<em>Field Type</em>'
	 * @see net.codecadenza.eclipse.model.client.FormField#getFieldType()
	 * @see #getFormField()
	 * @generated
	 */
	EAttribute getFormField_FieldType();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.client.FormField#getPanel
	 * <em>Panel</em>}'
	 * @return the meta object for the container reference '<em>Panel</em>'
	 * @see net.codecadenza.eclipse.model.client.FormField#getPanel()
	 * @see #getFormField()
	 * @generated
	 */
	EReference getFormField_Panel();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormField#getWidth <em>Width</em>}'
	 * @return the meta object for the attribute '<em>Width</em>'
	 * @see net.codecadenza.eclipse.model.client.FormField#getWidth()
	 * @see #getFormField()
	 * @generated
	 */
	EAttribute getFormField_Width();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormField#getDefaultValue <em>Default
	 * Value</em>}'
	 * @return the meta object for the attribute '<em>Default Value</em>'
	 * @see net.codecadenza.eclipse.model.client.FormField#getDefaultValue()
	 * @see #getFormField()
	 * @generated
	 */
	EAttribute getFormField_DefaultValue();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.FormField#getListOfValues <em>List Of
	 * Values</em>}'
	 * @return the meta object for the reference '<em>List Of Values</em>'
	 * @see net.codecadenza.eclipse.model.client.FormField#getListOfValues()
	 * @see #getFormField()
	 * @generated
	 */
	EReference getFormField_ListOfValues();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.FormField#getDTOAttribute <em>DTO
	 * Attribute</em>}'
	 * @return the meta object for the reference '<em>DTO Attribute</em>'
	 * @see net.codecadenza.eclipse.model.client.FormField#getDTOAttribute()
	 * @see #getFormField()
	 * @generated
	 */
	EReference getFormField_DTOAttribute();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormField#isAddFormLinkToLabel <em>Add
	 * Form Link To Label</em>}'
	 * @return the meta object for the attribute '<em>Add Form Link To Label</em>'
	 * @see net.codecadenza.eclipse.model.client.FormField#isAddFormLinkToLabel()
	 * @see #getFormField()
	 * @generated
	 */
	EAttribute getFormField_AddFormLinkToLabel();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.client.FormGroup <em>Form Group</em>}'
	 * @return the meta object for class '<em>Form Group</em>'
	 * @see net.codecadenza.eclipse.model.client.FormGroup
	 * @generated
	 */
	EClass getFormGroup();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormGroup#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getName()
	 * @see #getFormGroup()
	 * @generated
	 */
	EAttribute getFormGroup_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormGroup#getGroupOrder <em>Group
	 * Order</em>}'
	 * @return the meta object for the attribute '<em>Group Order</em>'
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getGroupOrder()
	 * @see #getFormGroup()
	 * @generated
	 */
	EAttribute getFormGroup_GroupOrder();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.FormGroup#getParentGroup <em>Parent
	 * Group</em>}'
	 * @return the meta object for the reference '<em>Parent Group</em>'
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getParentGroup()
	 * @see #getFormGroup()
	 * @generated
	 */
	EReference getFormGroup_ParentGroup();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.client.FormGroup#getChildGroups <em>Child Groups</em>}'
	 * @return the meta object for the containment reference list '<em>Child Groups</em>'
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getChildGroups()
	 * @see #getFormGroup()
	 * @generated
	 */
	EReference getFormGroup_ChildGroups();

	/**
	 * Return the meta object for the reference list '{@link net.codecadenza.eclipse.model.client.FormGroup#getForms
	 * <em>Forms</em>}'
	 * @return the meta object for the reference list '<em>Forms</em>'
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getForms()
	 * @see #getFormGroup()
	 * @generated
	 */
	EReference getFormGroup_Forms();

	/**
	 * Return the meta object for the reference list '{@link net.codecadenza.eclipse.model.client.FormGroup#getPanels
	 * <em>Panels</em>}'
	 * @return the meta object for the reference list '<em>Panels</em>'
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getPanels()
	 * @see #getFormGroup()
	 * @generated
	 */
	EReference getFormGroup_Panels();

	/**
	 * Return the meta object for the reference list '{@link net.codecadenza.eclipse.model.client.FormGroup#getRoles
	 * <em>Roles</em>}'
	 * @return the meta object for the reference list '<em>Roles</em>'
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getRoles()
	 * @see #getFormGroup()
	 * @generated
	 */
	EReference getFormGroup_Roles();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.FormGroup#getProject <em>Project</em>}'
	 * @return the meta object for the reference '<em>Project</em>'
	 * @see net.codecadenza.eclipse.model.client.FormGroup#getProject()
	 * @see #getFormGroup()
	 * @generated
	 */
	EReference getFormGroup_Project();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.client.FormPanel <em>Form Panel</em>}'
	 * @return the meta object for class '<em>Form Panel</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel
	 * @generated
	 */
	EClass getFormPanel();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormPanel#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getName()
	 * @see #getFormPanel()
	 * @generated
	 */
	EAttribute getFormPanel_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormPanel#getColumnCount <em>Column
	 * Count</em>}'
	 * @return the meta object for the attribute '<em>Column Count</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getColumnCount()
	 * @see #getFormPanel()
	 * @generated
	 */
	EAttribute getFormPanel_ColumnCount();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormPanel#getColIndex <em>Col
	 * Index</em>}'
	 * @return the meta object for the attribute '<em>Col Index</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getColIndex()
	 * @see #getFormPanel()
	 * @generated
	 */
	EAttribute getFormPanel_ColIndex();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormPanel#getRowIndex <em>Row
	 * Index</em>}'
	 * @return the meta object for the attribute '<em>Row Index</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getRowIndex()
	 * @see #getFormPanel()
	 * @generated
	 */
	EAttribute getFormPanel_RowIndex();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormPanel#isVerticalspan
	 * <em>Verticalspan</em>}'
	 * @return the meta object for the attribute '<em>Verticalspan</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel#isVerticalspan()
	 * @see #getFormPanel()
	 * @generated
	 */
	EAttribute getFormPanel_Verticalspan();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormPanel#isHorizontalSpan
	 * <em>Horizontal Span</em>}'
	 * @return the meta object for the attribute '<em>Horizontal Span</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel#isHorizontalSpan()
	 * @see #getFormPanel()
	 * @generated
	 */
	EAttribute getFormPanel_HorizontalSpan();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.FormPanel#getFormGroup <em>Form
	 * Group</em>}'
	 * @return the meta object for the reference '<em>Form Group</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getFormGroup()
	 * @see #getFormPanel()
	 * @generated
	 */
	EReference getFormPanel_FormGroup();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormPanel#getLabel <em>Label</em>}'
	 * @return the meta object for the attribute '<em>Label</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getLabel()
	 * @see #getFormPanel()
	 * @generated
	 */
	EAttribute getFormPanel_Label();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.FormPanel#getForm <em>Form</em>}'
	 * @return the meta object for the reference '<em>Form</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getForm()
	 * @see #getFormPanel()
	 * @generated
	 */
	EReference getFormPanel_Form();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormPanel#isDrawBorder <em>Draw
	 * Border</em>}'
	 * @return the meta object for the attribute '<em>Draw Border</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel#isDrawBorder()
	 * @see #getFormPanel()
	 * @generated
	 */
	EAttribute getFormPanel_DrawBorder();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.FormPanel#getBasePanel <em>Base
	 * Panel</em>}'
	 * @return the meta object for the reference '<em>Base Panel</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getBasePanel()
	 * @see #getFormPanel()
	 * @generated
	 */
	EReference getFormPanel_BasePanel();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.FormPanel#getFormTable <em>Form
	 * Table</em>}'
	 * @return the meta object for the reference '<em>Form Table</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getFormTable()
	 * @see #getFormPanel()
	 * @generated
	 */
	EReference getFormPanel_FormTable();

	/**
	 * Return the meta object for the containment reference list '{@link net.codecadenza.eclipse.model.client.FormPanel#getFields
	 * <em>Fields</em>}'
	 * @return the meta object for the containment reference list '<em>Fields</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getFields()
	 * @see #getFormPanel()
	 * @generated
	 */
	EReference getFormPanel_Fields();

	/**
	 * Return the meta object for the containment reference list '{@link net.codecadenza.eclipse.model.client.FormPanel#getActions
	 * <em>Actions</em>}'
	 * @return the meta object for the containment reference list '<em>Actions</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getActions()
	 * @see #getFormPanel()
	 * @generated
	 */
	EReference getFormPanel_Actions();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.FormPanel#getDTO <em>DTO</em>}'
	 * @return the meta object for the reference '<em>DTO</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getDTO()
	 * @see #getFormPanel()
	 * @generated
	 */
	EReference getFormPanel_DTO();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.FormPanel#getBoundaryMethod
	 * <em>Boundary Method</em>}'
	 * @return the meta object for the reference '<em>Boundary Method</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getBoundaryMethod()
	 * @see #getFormPanel()
	 * @generated
	 */
	EReference getFormPanel_BoundaryMethod();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.FormPanel#getAssociation
	 * <em>Association</em>}'
	 * @return the meta object for the reference '<em>Association</em>'
	 * @see net.codecadenza.eclipse.model.client.FormPanel#getAssociation()
	 * @see #getFormPanel()
	 * @generated
	 */
	EReference getFormPanel_Association();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.client.FormTable <em>Form Table</em>}'
	 * @return the meta object for class '<em>Form Table</em>'
	 * @see net.codecadenza.eclipse.model.client.FormTable
	 * @generated
	 */
	EClass getFormTable();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormTable#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.client.FormTable#getName()
	 * @see #getFormTable()
	 * @generated
	 */
	EAttribute getFormTable_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormTable#getColIndex <em>Col
	 * Index</em>}'
	 * @return the meta object for the attribute '<em>Col Index</em>'
	 * @see net.codecadenza.eclipse.model.client.FormTable#getColIndex()
	 * @see #getFormTable()
	 * @generated
	 */
	EAttribute getFormTable_ColIndex();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormTable#getRowIndex <em>Row
	 * Index</em>}'
	 * @return the meta object for the attribute '<em>Row Index</em>'
	 * @see net.codecadenza.eclipse.model.client.FormTable#getRowIndex()
	 * @see #getFormTable()
	 * @generated
	 */
	EAttribute getFormTable_RowIndex();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormTable#getSpanCols <em>Span
	 * Cols</em>}'
	 * @return the meta object for the attribute '<em>Span Cols</em>'
	 * @see net.codecadenza.eclipse.model.client.FormTable#getSpanCols()
	 * @see #getFormTable()
	 * @generated
	 */
	EAttribute getFormTable_SpanCols();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormTable#getSpanRows <em>Span
	 * Rows</em>}'
	 * @return the meta object for the attribute '<em>Span Rows</em>'
	 * @see net.codecadenza.eclipse.model.client.FormTable#getSpanRows()
	 * @see #getFormTable()
	 * @generated
	 */
	EAttribute getFormTable_SpanRows();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormTable#isVerticalSpan <em>Vertical
	 * Span</em>}'
	 * @return the meta object for the attribute '<em>Vertical Span</em>'
	 * @see net.codecadenza.eclipse.model.client.FormTable#isVerticalSpan()
	 * @see #getFormTable()
	 * @generated
	 */
	EAttribute getFormTable_VerticalSpan();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.FormTable#isHorizontalSpan
	 * <em>Horizontal Span</em>}'
	 * @return the meta object for the attribute '<em>Horizontal Span</em>'
	 * @see net.codecadenza.eclipse.model.client.FormTable#isHorizontalSpan()
	 * @see #getFormTable()
	 * @generated
	 */
	EAttribute getFormTable_HorizontalSpan();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.FormTable#getFormPanel <em>Form
	 * Panel</em>}'
	 * @return the meta object for the reference '<em>Form Panel</em>'
	 * @see net.codecadenza.eclipse.model.client.FormTable#getFormPanel()
	 * @see #getFormTable()
	 * @generated
	 */
	EReference getFormTable_FormPanel();

	/**
	 * Return the meta object for the containment reference list '{@link net.codecadenza.eclipse.model.client.FormTable#getFields
	 * <em>Fields</em>}'
	 * @return the meta object for the containment reference list '<em>Fields</em>'
	 * @see net.codecadenza.eclipse.model.client.FormTable#getFields()
	 * @see #getFormTable()
	 * @generated
	 */
	EReference getFormTable_Fields();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.FormTable#getAssociation
	 * <em>Association</em>}'
	 * @return the meta object for the reference '<em>Association</em>'
	 * @see net.codecadenza.eclipse.model.client.FormTable#getAssociation()
	 * @see #getFormTable()
	 * @generated
	 */
	EReference getFormTable_Association();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.client.TableColumnField <em>Table Column Field</em>}'
	 * @return the meta object for class '<em>Table Column Field</em>'
	 * @see net.codecadenza.eclipse.model.client.TableColumnField
	 * @generated
	 */
	EClass getTableColumnField();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.TableColumnField#getColIndex <em>Col
	 * Index</em>}'
	 * @return the meta object for the attribute '<em>Col Index</em>'
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#getColIndex()
	 * @see #getTableColumnField()
	 * @generated
	 */
	EAttribute getTableColumnField_ColIndex();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.TableColumnField#isVisible
	 * <em>Visible</em>}'
	 * @return the meta object for the attribute '<em>Visible</em>'
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#isVisible()
	 * @see #getTableColumnField()
	 * @generated
	 */
	EAttribute getTableColumnField_Visible();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.TableColumnField#isIdentifier
	 * <em>Identifier</em>}'
	 * @return the meta object for the attribute '<em>Identifier</em>'
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#isIdentifier()
	 * @see #getTableColumnField()
	 * @generated
	 */
	EAttribute getTableColumnField_Identifier();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.client.TableColumnField#getFormTable
	 * <em>Form Table</em>}'
	 * @return the meta object for the container reference '<em>Form Table</em>'
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#getFormTable()
	 * @see #getTableColumnField()
	 * @generated
	 */
	EReference getTableColumnField_FormTable();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.TableColumnField#getWidth
	 * <em>Width</em>}'
	 * @return the meta object for the attribute '<em>Width</em>'
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#getWidth()
	 * @see #getTableColumnField()
	 * @generated
	 */
	EAttribute getTableColumnField_Width();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.TableColumnField#getTitle
	 * <em>Title</em>}'
	 * @return the meta object for the attribute '<em>Title</em>'
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#getTitle()
	 * @see #getTableColumnField()
	 * @generated
	 */
	EAttribute getTableColumnField_Title();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.TableColumnField#getLovForm <em>Lov
	 * Form</em>}'
	 * @return the meta object for the reference '<em>Lov Form</em>'
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#getLovForm()
	 * @see #getTableColumnField()
	 * @generated
	 */
	EReference getTableColumnField_LovForm();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.TableColumnField#getFieldType <em>Field
	 * Type</em>}'
	 * @return the meta object for the attribute '<em>Field Type</em>'
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#getFieldType()
	 * @see #getTableColumnField()
	 * @generated
	 */
	EAttribute getTableColumnField_FieldType();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.TableColumnField#isAssociationRef
	 * <em>Association Ref</em>}'
	 * @return the meta object for the attribute '<em>Association Ref</em>'
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#isAssociationRef()
	 * @see #getTableColumnField()
	 * @generated
	 */
	EAttribute getTableColumnField_AssociationRef();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.TableColumnField#isSearchable
	 * <em>Searchable</em>}'
	 * @return the meta object for the attribute '<em>Searchable</em>'
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#isSearchable()
	 * @see #getTableColumnField()
	 * @generated
	 */
	EAttribute getTableColumnField_Searchable();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.TableColumnField#getDTOAttribute
	 * <em>DTO Attribute</em>}'
	 * @return the meta object for the reference '<em>DTO Attribute</em>'
	 * @see net.codecadenza.eclipse.model.client.TableColumnField#getDTOAttribute()
	 * @see #getTableColumnField()
	 * @generated
	 */
	EReference getTableColumnField_DTOAttribute();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.client.TreeView <em>Tree View</em>}'
	 * @return the meta object for class '<em>Tree View</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeView
	 * @generated
	 */
	EClass getTreeView();

	/**
	 * Return the meta object for the containment reference '{@link net.codecadenza.eclipse.model.client.TreeView#getRootTreeItem
	 * <em>Root Tree Item</em>}'
	 * @return the meta object for the containment reference '<em>Root Tree Item</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeView#getRootTreeItem()
	 * @see #getTreeView()
	 * @generated
	 */
	EReference getTreeView_RootTreeItem();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.client.TreeView#getQuickSearchItems <em>Quick Search Items</em>}'
	 * @return the meta object for the containment reference list '<em>Quick Search Items</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeView#getQuickSearchItems()
	 * @see #getTreeView()
	 * @generated
	 */
	EReference getTreeView_QuickSearchItems();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.client.TreeView#getAdvancedSearchItems <em>Advanced Search Items</em>}'
	 * @return the meta object for the containment reference list '<em>Advanced Search Items</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeView#getAdvancedSearchItems()
	 * @see #getTreeView()
	 * @generated
	 */
	EReference getTreeView_AdvancedSearchItems();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.TreeView#getCountMethod <em>Count
	 * Method</em>}'
	 * @return the meta object for the reference '<em>Count Method</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeView#getCountMethod()
	 * @see #getTreeView()
	 * @generated
	 */
	EReference getTreeView_CountMethod();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.TreeView#getRecursiveMethod
	 * <em>Recursive Method</em>}'
	 * @return the meta object for the reference '<em>Recursive Method</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeView#getRecursiveMethod()
	 * @see #getTreeView()
	 * @generated
	 */
	EReference getTreeView_RecursiveMethod();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.client.TreeViewItem <em>Tree View Item</em>}'
	 * @return the meta object for class '<em>Tree View Item</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem
	 * @generated
	 */
	EClass getTreeViewItem();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getParentItem
	 * <em>Parent Item</em>}'
	 * @return the meta object for the container reference '<em>Parent Item</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getParentItem()
	 * @see #getTreeViewItem()
	 * @generated
	 */
	EReference getTreeViewItem_ParentItem();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getChildren <em>Children</em>}'
	 * @return the meta object for the containment reference list '<em>Children</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getChildren()
	 * @see #getTreeViewItem()
	 * @generated
	 */
	EReference getTreeViewItem_Children();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getDataFetchMethod
	 * <em>Data Fetch Method</em>}'
	 * @return the meta object for the reference '<em>Data Fetch Method</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getDataFetchMethod()
	 * @see #getTreeViewItem()
	 * @generated
	 */
	EReference getTreeViewItem_DataFetchMethod();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getDropMethod <em>Drop
	 * Method</em>}'
	 * @return the meta object for the reference '<em>Drop Method</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getDropMethod()
	 * @see #getTreeViewItem()
	 * @generated
	 */
	EReference getTreeViewItem_DropMethod();

	/**
	 * Return the meta object for the reference list '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getDisplayAttributes
	 * <em>Display Attributes</em>}'
	 * @return the meta object for the reference list '<em>Display Attributes</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getDisplayAttributes()
	 * @see #getTreeViewItem()
	 * @generated
	 */
	EReference getTreeViewItem_DisplayAttributes();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getAssociation
	 * <em>Association</em>}'
	 * @return the meta object for the reference '<em>Association</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getAssociation()
	 * @see #getTreeViewItem()
	 * @generated
	 */
	EReference getTreeViewItem_Association();

	/**
	 * Return the meta object for the containment reference list '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getNodes
	 * <em>Nodes</em>}'
	 * @return the meta object for the containment reference list '<em>Nodes</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getNodes()
	 * @see #getTreeViewItem()
	 * @generated
	 */
	EReference getTreeViewItem_Nodes();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getLabel <em>Label</em>}'
	 * @return the meta object for the attribute '<em>Label</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getLabel()
	 * @see #getTreeViewItem()
	 * @generated
	 */
	EAttribute getTreeViewItem_Label();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getItemDTO <em>Item
	 * DTO</em>}'
	 * @return the meta object for the reference '<em>Item DTO</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getItemDTO()
	 * @see #getTreeViewItem()
	 * @generated
	 */
	EReference getTreeViewItem_ItemDTO();

	/**
	 * Return the meta object for the reference list
	 * '{@link net.codecadenza.eclipse.model.client.TreeViewItem#getInvisibleAttributes <em>Invisible Attributes</em>}'
	 * @return the meta object for the reference list '<em>Invisible Attributes</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeViewItem#getInvisibleAttributes()
	 * @see #getTreeViewItem()
	 * @generated
	 */
	EReference getTreeViewItem_InvisibleAttributes();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.client.TreeSearchItem <em>Tree Search Item</em>}'
	 * @return the meta object for class '<em>Tree Search Item</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeSearchItem
	 * @generated
	 */
	EClass getTreeSearchItem();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.TreeSearchItem#getLabel
	 * <em>Label</em>}'
	 * @return the meta object for the attribute '<em>Label</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeSearchItem#getLabel()
	 * @see #getTreeSearchItem()
	 * @generated
	 */
	EAttribute getTreeSearchItem_Label();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.TreeSearchItem#getDTOAttribute <em>DTO
	 * Attribute</em>}'
	 * @return the meta object for the reference '<em>DTO Attribute</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeSearchItem#getDTOAttribute()
	 * @see #getTreeSearchItem()
	 * @generated
	 */
	EReference getTreeSearchItem_DTOAttribute();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.client.TreeNode <em>Tree Node</em>}'
	 * @return the meta object for class '<em>Tree Node</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeNode
	 * @generated
	 */
	EClass getTreeNode();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.client.TreeNode#getLabel <em>Label</em>}'
	 * @return the meta object for the attribute '<em>Label</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeNode#getLabel()
	 * @see #getTreeNode()
	 * @generated
	 */
	EAttribute getTreeNode_Label();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.client.TreeNode#getDTOAttribute <em>DTO
	 * Attribute</em>}'
	 * @return the meta object for the reference '<em>DTO Attribute</em>'
	 * @see net.codecadenza.eclipse.model.client.TreeNode#getDTOAttribute()
	 * @see #getTreeNode()
	 * @generated
	 */
	EReference getTreeNode_DTOAttribute();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration <em>Form Field Type
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Form Field Type Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration
	 * @generated
	 */
	EEnum getFormFieldTypeEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.client.FormTypeEnumeration <em>Form Type
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Form Type Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.client.FormTypeEnumeration
	 * @generated
	 */
	EEnum getFormTypeEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration <em>Table Column
	 * Field Type Enumeration</em>}'
	 * @return the meta object for enum '<em>Table Column Field Type Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration
	 * @generated
	 */
	EEnum getTableColumnFieldTypeEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.client.ActionType <em>Action Type</em>}'
	 * @return the meta object for enum '<em>Action Type</em>'
	 * @see net.codecadenza.eclipse.model.client.ActionType
	 * @generated
	 */
	EEnum getActionType();

	/**
	 * @return the factory that creates the instances of the model
	 * @generated
	 */
	ClientFactory getClientFactory();

	/**
	 * Defines literals for the meta objects that represent
	 * <ul>
	 * <li>each class,</li>
	 * <li>each feature of each class,</li>
	 * <li>each enum,</li>
	 * <li>and each data type</li>
	 * </ul>
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.client.impl.FormImpl <em>Form</em>}' class
		 * @see net.codecadenza.eclipse.model.client.impl.FormImpl
		 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getForm()
		 * @generated
		 */
		EClass FORM = eINSTANCE.getForm();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM__NAME = eINSTANCE.getForm_Name();

		/**
		 * The meta object literal for the '<em><b>Form Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM__FORM_TYPE = eINSTANCE.getForm_FormType();

		/**
		 * The meta object literal for the '<em><b>Form Group</b></em>' reference feature
		 * @generated
		 */
		EReference FORM__FORM_GROUP = eINSTANCE.getForm_FormGroup();

		/**
		 * The meta object literal for the '<em><b>Title</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM__TITLE = eINSTANCE.getForm_Title();

		/**
		 * The meta object literal for the '<em><b>Modal</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM__MODAL = eINSTANCE.getForm_Modal();

		/**
		 * The meta object literal for the '<em><b>Resizable</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM__RESIZABLE = eINSTANCE.getForm_Resizable();

		/**
		 * The meta object literal for the '<em><b>Title Area</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM__TITLE_AREA = eINSTANCE.getForm_TitleArea();

		/**
		 * The meta object literal for the '<em><b>Open Edit After Create</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM__OPEN_EDIT_AFTER_CREATE = eINSTANCE.getForm_OpenEditAfterCreate();

		/**
		 * The meta object literal for the '<em><b>Height</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM__HEIGHT = eINSTANCE.getForm_Height();

		/**
		 * The meta object literal for the '<em><b>Width</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM__WIDTH = eINSTANCE.getForm_Width();

		/**
		 * The meta object literal for the '<em><b>Form Panels</b></em>' reference list feature
		 * @generated
		 */
		EReference FORM__FORM_PANELS = eINSTANCE.getForm_FormPanels();

		/**
		 * The meta object literal for the '<em><b>Actions</b></em>' containment reference list feature
		 * @generated
		 */
		EReference FORM__ACTIONS = eINSTANCE.getForm_Actions();

		/**
		 * The meta object literal for the '<em><b>DTO</b></em>' reference feature
		 * @generated
		 */
		EReference FORM__DTO = eINSTANCE.getForm_DTO();

		/**
		 * The meta object literal for the '<em><b>Domain Object</b></em>' reference feature
		 * @generated
		 */
		EReference FORM__DOMAIN_OBJECT = eINSTANCE.getForm_DomainObject();

		/**
		 * The meta object literal for the '<em><b>Roles</b></em>' reference list feature
		 * @generated
		 */
		EReference FORM__ROLES = eINSTANCE.getForm_Roles();

		/**
		 * The meta object literal for the '<em><b>Boundary Method</b></em>' reference feature
		 * @generated
		 */
		EReference FORM__BOUNDARY_METHOD = eINSTANCE.getForm_BoundaryMethod();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.client.impl.FormActionImpl <em>Form Action</em>}'
		 * class
		 * @see net.codecadenza.eclipse.model.client.impl.FormActionImpl
		 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getFormAction()
		 * @generated
		 */
		EClass FORM_ACTION = eINSTANCE.getFormAction();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_ACTION__NAME = eINSTANCE.getFormAction_Name();

		/**
		 * The meta object literal for the '<em><b>Description</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_ACTION__DESCRIPTION = eINSTANCE.getFormAction_Description();

		/**
		 * The meta object literal for the '<em><b>Form</b></em>' container reference feature
		 * @generated
		 */
		EReference FORM_ACTION__FORM = eINSTANCE.getFormAction_Form();

		/**
		 * The meta object literal for the '<em><b>Panel</b></em>' container reference feature
		 * @generated
		 */
		EReference FORM_ACTION__PANEL = eINSTANCE.getFormAction_Panel();

		/**
		 * The meta object literal for the '<em><b>Target Form</b></em>' reference feature
		 * @generated
		 */
		EReference FORM_ACTION__TARGET_FORM = eINSTANCE.getFormAction_TargetForm();

		/**
		 * The meta object literal for the '<em><b>Boundary Method</b></em>' reference feature
		 * @generated
		 */
		EReference FORM_ACTION__BOUNDARY_METHOD = eINSTANCE.getFormAction_BoundaryMethod();

		/**
		 * The meta object literal for the '<em><b>Roles</b></em>' reference list feature
		 * @generated
		 */
		EReference FORM_ACTION__ROLES = eINSTANCE.getFormAction_Roles();

		/**
		 * The meta object literal for the '<em><b>Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_ACTION__TYPE = eINSTANCE.getFormAction_Type();

		/**
		 * The meta object literal for the '<em><b>Label</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_ACTION__LABEL = eINSTANCE.getFormAction_Label();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.client.impl.FormFieldImpl <em>Form Field</em>}' class
		 * @see net.codecadenza.eclipse.model.client.impl.FormFieldImpl
		 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getFormField()
		 * @generated
		 */
		EClass FORM_FIELD = eINSTANCE.getFormField();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_FIELD__NAME = eINSTANCE.getFormField_Name();

		/**
		 * The meta object literal for the '<em><b>Col Index</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_FIELD__COL_INDEX = eINSTANCE.getFormField_ColIndex();

		/**
		 * The meta object literal for the '<em><b>Row Index</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_FIELD__ROW_INDEX = eINSTANCE.getFormField_RowIndex();

		/**
		 * The meta object literal for the '<em><b>Visible</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_FIELD__VISIBLE = eINSTANCE.getFormField_Visible();

		/**
		 * The meta object literal for the '<em><b>Label</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_FIELD__LABEL = eINSTANCE.getFormField_Label();

		/**
		 * The meta object literal for the '<em><b>Span Cols</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_FIELD__SPAN_COLS = eINSTANCE.getFormField_SpanCols();

		/**
		 * The meta object literal for the '<em><b>Readonly</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_FIELD__READONLY = eINSTANCE.getFormField_Readonly();

		/**
		 * The meta object literal for the '<em><b>Mandatory</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_FIELD__MANDATORY = eINSTANCE.getFormField_Mandatory();

		/**
		 * The meta object literal for the '<em><b>Field Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_FIELD__FIELD_TYPE = eINSTANCE.getFormField_FieldType();

		/**
		 * The meta object literal for the '<em><b>Panel</b></em>' container reference feature
		 * @generated
		 */
		EReference FORM_FIELD__PANEL = eINSTANCE.getFormField_Panel();

		/**
		 * The meta object literal for the '<em><b>Width</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_FIELD__WIDTH = eINSTANCE.getFormField_Width();

		/**
		 * The meta object literal for the '<em><b>Default Value</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_FIELD__DEFAULT_VALUE = eINSTANCE.getFormField_DefaultValue();

		/**
		 * The meta object literal for the '<em><b>List Of Values</b></em>' reference feature
		 * @generated
		 */
		EReference FORM_FIELD__LIST_OF_VALUES = eINSTANCE.getFormField_ListOfValues();

		/**
		 * The meta object literal for the '<em><b>DTO Attribute</b></em>' reference feature
		 * @generated
		 */
		EReference FORM_FIELD__DTO_ATTRIBUTE = eINSTANCE.getFormField_DTOAttribute();

		/**
		 * The meta object literal for the '<em><b>Add Form Link To Label</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_FIELD__ADD_FORM_LINK_TO_LABEL = eINSTANCE.getFormField_AddFormLinkToLabel();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.client.impl.FormGroupImpl <em>Form Group</em>}' class
		 * @see net.codecadenza.eclipse.model.client.impl.FormGroupImpl
		 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getFormGroup()
		 * @generated
		 */
		EClass FORM_GROUP = eINSTANCE.getFormGroup();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_GROUP__NAME = eINSTANCE.getFormGroup_Name();

		/**
		 * The meta object literal for the '<em><b>Group Order</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_GROUP__GROUP_ORDER = eINSTANCE.getFormGroup_GroupOrder();

		/**
		 * The meta object literal for the '<em><b>Parent Group</b></em>' reference feature
		 * @generated
		 */
		EReference FORM_GROUP__PARENT_GROUP = eINSTANCE.getFormGroup_ParentGroup();

		/**
		 * The meta object literal for the '<em><b>Child Groups</b></em>' containment reference list feature
		 * @generated
		 */
		EReference FORM_GROUP__CHILD_GROUPS = eINSTANCE.getFormGroup_ChildGroups();

		/**
		 * The meta object literal for the '<em><b>Forms</b></em>' reference list feature
		 * @generated
		 */
		EReference FORM_GROUP__FORMS = eINSTANCE.getFormGroup_Forms();

		/**
		 * The meta object literal for the '<em><b>Panels</b></em>' reference list feature
		 * @generated
		 */
		EReference FORM_GROUP__PANELS = eINSTANCE.getFormGroup_Panels();

		/**
		 * The meta object literal for the '<em><b>Roles</b></em>' reference list feature
		 * @generated
		 */
		EReference FORM_GROUP__ROLES = eINSTANCE.getFormGroup_Roles();

		/**
		 * The meta object literal for the '<em><b>Project</b></em>' reference feature
		 * @generated
		 */
		EReference FORM_GROUP__PROJECT = eINSTANCE.getFormGroup_Project();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.client.impl.FormPanelImpl <em>Form Panel</em>}' class
		 * @see net.codecadenza.eclipse.model.client.impl.FormPanelImpl
		 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getFormPanel()
		 * @generated
		 */
		EClass FORM_PANEL = eINSTANCE.getFormPanel();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_PANEL__NAME = eINSTANCE.getFormPanel_Name();

		/**
		 * The meta object literal for the '<em><b>Column Count</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_PANEL__COLUMN_COUNT = eINSTANCE.getFormPanel_ColumnCount();

		/**
		 * The meta object literal for the '<em><b>Col Index</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_PANEL__COL_INDEX = eINSTANCE.getFormPanel_ColIndex();

		/**
		 * The meta object literal for the '<em><b>Row Index</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_PANEL__ROW_INDEX = eINSTANCE.getFormPanel_RowIndex();

		/**
		 * The meta object literal for the '<em><b>Verticalspan</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_PANEL__VERTICALSPAN = eINSTANCE.getFormPanel_Verticalspan();

		/**
		 * The meta object literal for the '<em><b>Horizontal Span</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_PANEL__HORIZONTAL_SPAN = eINSTANCE.getFormPanel_HorizontalSpan();

		/**
		 * The meta object literal for the '<em><b>Form Group</b></em>' reference feature
		 * @generated
		 */
		EReference FORM_PANEL__FORM_GROUP = eINSTANCE.getFormPanel_FormGroup();

		/**
		 * The meta object literal for the '<em><b>Label</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_PANEL__LABEL = eINSTANCE.getFormPanel_Label();

		/**
		 * The meta object literal for the '<em><b>Form</b></em>' reference feature
		 * @generated
		 */
		EReference FORM_PANEL__FORM = eINSTANCE.getFormPanel_Form();

		/**
		 * The meta object literal for the '<em><b>Draw Border</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_PANEL__DRAW_BORDER = eINSTANCE.getFormPanel_DrawBorder();

		/**
		 * The meta object literal for the '<em><b>Base Panel</b></em>' reference feature
		 * @generated
		 */
		EReference FORM_PANEL__BASE_PANEL = eINSTANCE.getFormPanel_BasePanel();

		/**
		 * The meta object literal for the '<em><b>Form Table</b></em>' reference feature
		 * @generated
		 */
		EReference FORM_PANEL__FORM_TABLE = eINSTANCE.getFormPanel_FormTable();

		/**
		 * The meta object literal for the '<em><b>Fields</b></em>' containment reference list feature
		 * @generated
		 */
		EReference FORM_PANEL__FIELDS = eINSTANCE.getFormPanel_Fields();

		/**
		 * The meta object literal for the '<em><b>Actions</b></em>' containment reference list feature
		 * @generated
		 */
		EReference FORM_PANEL__ACTIONS = eINSTANCE.getFormPanel_Actions();

		/**
		 * The meta object literal for the '<em><b>DTO</b></em>' reference feature
		 * @generated
		 */
		EReference FORM_PANEL__DTO = eINSTANCE.getFormPanel_DTO();

		/**
		 * The meta object literal for the '<em><b>Boundary Method</b></em>' reference feature
		 * @generated
		 */
		EReference FORM_PANEL__BOUNDARY_METHOD = eINSTANCE.getFormPanel_BoundaryMethod();

		/**
		 * The meta object literal for the '<em><b>Association</b></em>' reference feature
		 * @generated
		 */
		EReference FORM_PANEL__ASSOCIATION = eINSTANCE.getFormPanel_Association();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.client.impl.FormTableImpl <em>Form Table</em>}' class
		 * @see net.codecadenza.eclipse.model.client.impl.FormTableImpl
		 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getFormTable()
		 * @generated
		 */
		EClass FORM_TABLE = eINSTANCE.getFormTable();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_TABLE__NAME = eINSTANCE.getFormTable_Name();

		/**
		 * The meta object literal for the '<em><b>Col Index</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_TABLE__COL_INDEX = eINSTANCE.getFormTable_ColIndex();

		/**
		 * The meta object literal for the '<em><b>Row Index</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_TABLE__ROW_INDEX = eINSTANCE.getFormTable_RowIndex();

		/**
		 * The meta object literal for the '<em><b>Span Cols</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_TABLE__SPAN_COLS = eINSTANCE.getFormTable_SpanCols();

		/**
		 * The meta object literal for the '<em><b>Span Rows</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_TABLE__SPAN_ROWS = eINSTANCE.getFormTable_SpanRows();

		/**
		 * The meta object literal for the '<em><b>Vertical Span</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_TABLE__VERTICAL_SPAN = eINSTANCE.getFormTable_VerticalSpan();

		/**
		 * The meta object literal for the '<em><b>Horizontal Span</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FORM_TABLE__HORIZONTAL_SPAN = eINSTANCE.getFormTable_HorizontalSpan();

		/**
		 * The meta object literal for the '<em><b>Form Panel</b></em>' reference feature
		 * @generated
		 */
		EReference FORM_TABLE__FORM_PANEL = eINSTANCE.getFormTable_FormPanel();

		/**
		 * The meta object literal for the '<em><b>Fields</b></em>' containment reference list feature
		 * @generated
		 */
		EReference FORM_TABLE__FIELDS = eINSTANCE.getFormTable_Fields();

		/**
		 * The meta object literal for the '<em><b>Association</b></em>' reference feature
		 * @generated
		 */
		EReference FORM_TABLE__ASSOCIATION = eINSTANCE.getFormTable_Association();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.client.impl.TableColumnFieldImpl <em>Table Column
		 * Field</em>}' class
		 * @see net.codecadenza.eclipse.model.client.impl.TableColumnFieldImpl
		 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getTableColumnField()
		 * @generated
		 */
		EClass TABLE_COLUMN_FIELD = eINSTANCE.getTableColumnField();

		/**
		 * The meta object literal for the '<em><b>Col Index</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TABLE_COLUMN_FIELD__COL_INDEX = eINSTANCE.getTableColumnField_ColIndex();

		/**
		 * The meta object literal for the '<em><b>Visible</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TABLE_COLUMN_FIELD__VISIBLE = eINSTANCE.getTableColumnField_Visible();

		/**
		 * The meta object literal for the '<em><b>Identifier</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TABLE_COLUMN_FIELD__IDENTIFIER = eINSTANCE.getTableColumnField_Identifier();

		/**
		 * The meta object literal for the '<em><b>Form Table</b></em>' container reference feature
		 * @generated
		 */
		EReference TABLE_COLUMN_FIELD__FORM_TABLE = eINSTANCE.getTableColumnField_FormTable();

		/**
		 * The meta object literal for the '<em><b>Width</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TABLE_COLUMN_FIELD__WIDTH = eINSTANCE.getTableColumnField_Width();

		/**
		 * The meta object literal for the '<em><b>Title</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TABLE_COLUMN_FIELD__TITLE = eINSTANCE.getTableColumnField_Title();

		/**
		 * The meta object literal for the '<em><b>Lov Form</b></em>' reference feature
		 * @generated
		 */
		EReference TABLE_COLUMN_FIELD__LOV_FORM = eINSTANCE.getTableColumnField_LovForm();

		/**
		 * The meta object literal for the '<em><b>Field Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TABLE_COLUMN_FIELD__FIELD_TYPE = eINSTANCE.getTableColumnField_FieldType();

		/**
		 * The meta object literal for the '<em><b>Association Ref</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TABLE_COLUMN_FIELD__ASSOCIATION_REF = eINSTANCE.getTableColumnField_AssociationRef();

		/**
		 * The meta object literal for the '<em><b>Searchable</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TABLE_COLUMN_FIELD__SEARCHABLE = eINSTANCE.getTableColumnField_Searchable();

		/**
		 * The meta object literal for the '<em><b>DTO Attribute</b></em>' reference feature
		 * @generated
		 */
		EReference TABLE_COLUMN_FIELD__DTO_ATTRIBUTE = eINSTANCE.getTableColumnField_DTOAttribute();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.client.impl.TreeViewImpl <em>Tree View</em>}' class
		 * @see net.codecadenza.eclipse.model.client.impl.TreeViewImpl
		 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getTreeView()
		 * @generated
		 */
		EClass TREE_VIEW = eINSTANCE.getTreeView();

		/**
		 * The meta object literal for the '<em><b>Root Tree Item</b></em>' containment reference feature
		 * @generated
		 */
		EReference TREE_VIEW__ROOT_TREE_ITEM = eINSTANCE.getTreeView_RootTreeItem();

		/**
		 * The meta object literal for the '<em><b>Quick Search Items</b></em>' containment reference list feature
		 * @generated
		 */
		EReference TREE_VIEW__QUICK_SEARCH_ITEMS = eINSTANCE.getTreeView_QuickSearchItems();

		/**
		 * The meta object literal for the '<em><b>Advanced Search Items</b></em>' containment reference list feature
		 * @generated
		 */
		EReference TREE_VIEW__ADVANCED_SEARCH_ITEMS = eINSTANCE.getTreeView_AdvancedSearchItems();

		/**
		 * The meta object literal for the '<em><b>Count Method</b></em>' reference feature
		 * @generated
		 */
		EReference TREE_VIEW__COUNT_METHOD = eINSTANCE.getTreeView_CountMethod();

		/**
		 * The meta object literal for the '<em><b>Recursive Method</b></em>' reference feature
		 * @generated
		 */
		EReference TREE_VIEW__RECURSIVE_METHOD = eINSTANCE.getTreeView_RecursiveMethod();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.client.impl.TreeViewItemImpl <em>Tree View
		 * Item</em>}' class
		 * @see net.codecadenza.eclipse.model.client.impl.TreeViewItemImpl
		 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getTreeViewItem()
		 * @generated
		 */
		EClass TREE_VIEW_ITEM = eINSTANCE.getTreeViewItem();

		/**
		 * The meta object literal for the '<em><b>Parent Item</b></em>' container reference feature
		 * @generated
		 */
		EReference TREE_VIEW_ITEM__PARENT_ITEM = eINSTANCE.getTreeViewItem_ParentItem();

		/**
		 * The meta object literal for the '<em><b>Children</b></em>' containment reference list feature
		 * @generated
		 */
		EReference TREE_VIEW_ITEM__CHILDREN = eINSTANCE.getTreeViewItem_Children();

		/**
		 * The meta object literal for the '<em><b>Data Fetch Method</b></em>' reference feature
		 * @generated
		 */
		EReference TREE_VIEW_ITEM__DATA_FETCH_METHOD = eINSTANCE.getTreeViewItem_DataFetchMethod();

		/**
		 * The meta object literal for the '<em><b>Drop Method</b></em>' reference feature
		 * @generated
		 */
		EReference TREE_VIEW_ITEM__DROP_METHOD = eINSTANCE.getTreeViewItem_DropMethod();

		/**
		 * The meta object literal for the '<em><b>Display Attributes</b></em>' reference list feature
		 * @generated
		 */
		EReference TREE_VIEW_ITEM__DISPLAY_ATTRIBUTES = eINSTANCE.getTreeViewItem_DisplayAttributes();

		/**
		 * The meta object literal for the '<em><b>Association</b></em>' reference feature
		 * @generated
		 */
		EReference TREE_VIEW_ITEM__ASSOCIATION = eINSTANCE.getTreeViewItem_Association();

		/**
		 * The meta object literal for the '<em><b>Nodes</b></em>' containment reference list feature
		 * @generated
		 */
		EReference TREE_VIEW_ITEM__NODES = eINSTANCE.getTreeViewItem_Nodes();

		/**
		 * The meta object literal for the '<em><b>Label</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TREE_VIEW_ITEM__LABEL = eINSTANCE.getTreeViewItem_Label();

		/**
		 * The meta object literal for the '<em><b>Item DTO</b></em>' reference feature
		 * @generated
		 */
		EReference TREE_VIEW_ITEM__ITEM_DTO = eINSTANCE.getTreeViewItem_ItemDTO();

		/**
		 * The meta object literal for the '<em><b>Invisible Attributes</b></em>' reference list feature
		 * @generated
		 */
		EReference TREE_VIEW_ITEM__INVISIBLE_ATTRIBUTES = eINSTANCE.getTreeViewItem_InvisibleAttributes();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.client.impl.TreeSearchItemImpl <em>Tree Search
		 * Item</em>}' class
		 * @see net.codecadenza.eclipse.model.client.impl.TreeSearchItemImpl
		 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getTreeSearchItem()
		 * @generated
		 */
		EClass TREE_SEARCH_ITEM = eINSTANCE.getTreeSearchItem();

		/**
		 * The meta object literal for the '<em><b>Label</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TREE_SEARCH_ITEM__LABEL = eINSTANCE.getTreeSearchItem_Label();

		/**
		 * The meta object literal for the '<em><b>DTO Attribute</b></em>' reference feature
		 * @generated
		 */
		EReference TREE_SEARCH_ITEM__DTO_ATTRIBUTE = eINSTANCE.getTreeSearchItem_DTOAttribute();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.client.impl.TreeNodeImpl <em>Tree Node</em>}' class
		 * @see net.codecadenza.eclipse.model.client.impl.TreeNodeImpl
		 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getTreeNode()
		 * @generated
		 */
		EClass TREE_NODE = eINSTANCE.getTreeNode();

		/**
		 * The meta object literal for the '<em><b>Label</b></em>' attribute feature
		 * @generated
		 */
		EAttribute TREE_NODE__LABEL = eINSTANCE.getTreeNode_Label();

		/**
		 * The meta object literal for the '<em><b>DTO Attribute</b></em>' reference feature
		 * @generated
		 */
		EReference TREE_NODE__DTO_ATTRIBUTE = eINSTANCE.getTreeNode_DTOAttribute();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration <em>Form Field Type
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration
		 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getFormFieldTypeEnumeration()
		 * @generated
		 */
		EEnum FORM_FIELD_TYPE_ENUMERATION = eINSTANCE.getFormFieldTypeEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.client.FormTypeEnumeration <em>Form Type
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.client.FormTypeEnumeration
		 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getFormTypeEnumeration()
		 * @generated
		 */
		EEnum FORM_TYPE_ENUMERATION = eINSTANCE.getFormTypeEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration <em>Table
		 * Column Field Type Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration
		 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getTableColumnFieldTypeEnumeration()
		 * @generated
		 */
		EEnum TABLE_COLUMN_FIELD_TYPE_ENUMERATION = eINSTANCE.getTableColumnFieldTypeEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.client.ActionType <em>Action Type</em>}' enum
		 * @see net.codecadenza.eclipse.model.client.ActionType
		 * @see net.codecadenza.eclipse.model.client.impl.ClientPackageImpl#getActionType()
		 * @generated
		 */
		EEnum ACTION_TYPE = eINSTANCE.getActionType();

	}

}
