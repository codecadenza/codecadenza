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
package net.codecadenza.eclipse.ui.view;

import static net.codecadenza.eclipse.shared.Constants.DIAGRAM_FILE_EXTENSION;
import static net.codecadenza.eclipse.shared.Constants.IMG_ATTRIBUTE;
import static net.codecadenza.eclipse.shared.Constants.IMG_COLUMN;
import static net.codecadenza.eclipse.shared.Constants.IMG_ENUM_LITERAL;
import static net.codecadenza.eclipse.shared.Constants.IMG_KEY;
import static net.codecadenza.eclipse.shared.Constants.IMG_MTM_ASSOC;
import static net.codecadenza.eclipse.shared.Constants.IMG_MTO_ASSOC;
import static net.codecadenza.eclipse.shared.Constants.IMG_OTM_ASSOC;
import static net.codecadenza.eclipse.shared.Constants.IMG_OTO_ASSOC;
import static net.codecadenza.eclipse.shared.Constants.IMG_PANEL;
import static net.codecadenza.eclipse.shared.Constants.IMG_REFRESH;
import static net.codecadenza.eclipse.shared.Constants.IMG_TABLE;
import static net.codecadenza.eclipse.shared.Constants.IMG_WINDOW_ADD;
import static net.codecadenza.eclipse.shared.Constants.IMG_WINDOW_CREATE;
import static net.codecadenza.eclipse.shared.Constants.IMG_WINDOW_EDIT;
import static net.codecadenza.eclipse.shared.Constants.IMG_WINDOW_LOV;
import static net.codecadenza.eclipse.shared.Constants.IMG_WINDOW_READ;
import static net.codecadenza.eclipse.shared.Constants.IMG_WINDOW_VIEW;
import static net.codecadenza.eclipse.shared.Constants.MODEL_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.MODEL_ROOT_FILE;
import static net.codecadenza.eclipse.shared.Constants.QUOTE;

import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormComparator;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormGroupComparator;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.PanelComparator;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.DBTableComparator;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainNamespace;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeComparator;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.java.NamespaceComparator;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.testing.AbstractTestCase;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.SeleniumTestModule;
import net.codecadenza.eclipse.model.testing.TestSuite;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.exchange.DataExchangeBeanService;
import net.codecadenza.eclipse.service.integration.IntegrationBeanService;
import net.codecadenza.eclipse.service.repository.RepositoryService;
import net.codecadenza.eclipse.shared.Constants;
import net.codecadenza.eclipse.shared.event.GraphicalEditorEventController;
import net.codecadenza.eclipse.shared.event.GraphicalEditorEventListener;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.exchange.EditExchangeMethodDialog;
import net.codecadenza.eclipse.ui.dialog.integration.EditIntegrationMethodDialog;
import net.codecadenza.eclipse.ui.view.menu.BoundaryBeanMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.BoundaryMethodMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.BoundaryNamespaceMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.ChildNamespaceMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.DTOBeanMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.DTONamespaceMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.DataExchangeMethodMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.DataExchangeServiceBeanMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.DatabaseMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.DomainNamespaceMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.DomainObjectMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.ExchangeMappingObjectMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.ExchangeNamespaceMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.FormGroupFolderMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.FormGroupMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.FormMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.GUITestCaseMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.GUITestModuleNamespaceMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.IntegrationBeanMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.IntegrationMethodMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.IntegrationNamespaceMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.IntegrationTestCaseMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.IntegrationTestNamespaceMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.JavaEnumMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.PanelMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.ProjectMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.RepositoryMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.RepositoryMethodMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.RepositoryNamespaceMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.TableMenuBuilder;
import net.codecadenza.eclipse.ui.view.menu.TestSuiteMenuBuilder;
import net.codecadenza.eclipse.ui.view.util.ProjectTreeViewHelper;
import net.codecadenza.eclipse.ui.view.util.TreeSelectionHelper;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.xmi.XMLResource;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.DropTargetListener;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.TreeAdapter;
import org.eclipse.swt.events.TreeEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.ViewPart;

/**
 * <p>
 * View that contains all projects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ProjectView extends ViewPart implements GraphicalEditorEventListener, IResourceChangeListener {
	public static final String REPOSITORY_NAMESPACE_LABEL = "Repositories";
	public static final String DTO_NAMESPACE_LABEL = "Data transfer objects";
	public static final String BOUNDARY_NAMESPACE_LABEL = "Boundary beans";
	public static final String FACADE_NAMESPACE_LABEL = "Facade beans";
	public static final String DATABASE_LABEL = "Database";
	public static final String FORM_GROUPS_LABEL = "Form groups";
	public static final String DOMAIN_NAMESPACE_LABEL = "Domain objects";
	public static final String DATA_EXCHANGE_NAMESPACE_LABEL = "Data exchange";
	public static final String INTEGRATION_LABEL = "Integration objects";
	public static final String TEST_MODULES_LABEL = "Test modules";
	public static final String TEST_MODULE_NAME_SELENIUM = "Selenium";
	public static final String TEST_MODULE_NAME_UNDEF = "UNDEF";

	private Tree tree;
	private ProjectView thisView;
	private Shell shell;
	private RefreshAction refreshAction;
	private TreeSelectionHelper currentSelection;

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.shared.event.GraphicalEditorEventListener#onEditorSaveEvent()
	 */
	@Override
	public void onEditorSaveEvent() {
		refreshTree();
	}

	/**
	 * Refresh action
	 */
	private class RefreshAction extends Action {
		/**
		 * Constructor
		 */
		public RefreshAction() {
			super("", Action.AS_PUSH_BUTTON);

			this.setToolTipText("Refresh view");
			this.setImageDescriptor(ImageDescriptor.createFromImage(CodeCadenzaResourcePlugin.getImage(IMG_REFRESH)));
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.action.Action#run()
		 */
		@Override
		public void run() {
			refreshTree();
		}
	}

	/**
	 * Refresh the tree view
	 */
	public void refreshTree() {
		tree.removeAll();

		try {
			thisView.buildTree();
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
		}
	}

	/**
	 * Add tables to the database tree item
	 * @param parentItem
	 */
	public void addDatabaseItems(TreeItem parentItem) {
		try {
			final var db = (Database) parentItem.getData();

			// Remove the dummy item
			parentItem.removeAll();

			final Collection<DomainObject> allDomainObjects = db.getProject().getAllDomainObjectsOfProject(true, true);

			// Add all tables of the target data source
			final EList<DBTable> tables = db.getDatabaseTables();

			ECollections.sort(tables, new DBTableComparator());

			for (final DBTable table : tables) {
				// Check if the table really exists! Tables for mapped superclasses should not be displayed!
				boolean addToTree = false;

				for (final DomainObject domainObj : allDomainObjects) {
					if (!table.equals(domainObj.getDatabaseTable()))
						continue;

					if (domainObj.isMappedSuperClass() || domainObj.getPKAttribute() == null)
						continue;

					addToTree = true;
					break;
				}

				if (!addToTree)
					for (final DomainObject domainObj : allDomainObjects)
						for (final DomainAttribute attr : domainObj.getAllAttributes())
							if (table.equals(attr.getCollectionTable())) {
								addToTree = true;
								break;
							}

				if (!addToTree)
					continue;

				final var tableItem = new TreeItem(parentItem, SWT.NONE);
				tableItem.setText(table.getFullDatabaseName().replace(QUOTE, ""));
				tableItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_TABLE));
				tableItem.setData(table);

				ProjectTreeViewHelper.addDatabaseTable(table.hashCode());

				// Add a dummy item
				new TreeItem(tableItem, SWT.NONE);
			}
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
		}
	}

	/**
	 * Add the columns to the table tree item
	 * @param parentItem
	 */
	public void addTableItems(TreeItem parentItem) {
		try {
			final var dbTable = (DBTable) parentItem.getData();

			// Remove the dummy item
			parentItem.removeAll();

			if (dbTable.getPrimaryKey() != null) {
				final var item = new TreeItem(parentItem, SWT.NONE);
				item.setText(dbTable.getPrimaryKey().getColumn().getConvertedName() + " ["
						+ dbTable.getPrimaryKey().getColumn().getColumnType().getName() + "]");
				item.setData(dbTable.getPrimaryKey().getColumn());
				item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_KEY));
			}

			// Add all tables of the target data source
			for (final DBColumn col : dbTable.getColumns()) {
				if (dbTable.getPrimaryKey() != null && dbTable.getPrimaryKey().getColumn().equals(col))
					continue;

				final var tableItem = new TreeItem(parentItem, SWT.NONE);
				tableItem.setText(col.getConvertedName() + " [" + col.getColumnType().getName() + "]");
				tableItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_COLUMN));
				tableItem.setData(col);
			}
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
		}
	}

	/**
	 * Add child items of this namespace to the tree view
	 * @param parentItem
	 */
	public void addNamespaceItems(TreeItem parentItem) {
		try {
			// Remove the dummy item
			parentItem.removeAll();

			final var namespace = (Namespace) parentItem.getData();

			// Add all sub-namespaces
			final EList<Namespace> namespaces = namespace.getChildNamespaces();
			ECollections.sort(namespaces, new NamespaceComparator());

			namespaces.forEach(ns -> {
				final var item = new TreeItem(parentItem, SWT.NONE);
				item.setText(ns.getName());
				item.setData(ns);
				item.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PACKAGE));

				// Add a dummy item
				new TreeItem(item, SWT.NONE);

				ProjectTreeViewHelper.addNamespace(ns.toString());
			});

			// Add all Java types of this namespace
			final EList<JavaType> types = namespace.getJavaTypes();
			ECollections.sort(types, new JavaTypeComparator());

			for (final JavaType t : types) {
				// Hide virtual DTOs
				if (t instanceof final DTOBean dto && dto.isVirtual())
					continue;

				// Hide domain objects that do not have a valid primary key attribute!
				if (t instanceof final DomainObject domainObject && domainObject.getPKAttribute() == null)
					continue;

				final var item = new TreeItem(parentItem, SWT.NONE);
				item.setText(t.getName());
				item.setData(t);

				if (t.getClass() == JavaEnum.class)
					item.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_ENUM));
				else
					item.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_CLASS));

				if (t instanceof final Repository repository)
					item.setText(repository.getName());

				if (t instanceof final BoundaryBean boundary) {
					final Project proj = t.getNamespace().getProject();

					if (!proj.isAddBoundaryInterface())
						item.setText(boundary.getInterfaceName());
				}

				// Add a dummy item
				if (!(t instanceof AbstractTestCase) && !(t instanceof TestSuite))
					new TreeItem(item, SWT.NONE);

				ProjectTreeViewHelper.addJavaType(t.hashCode());
			}
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
		}
	}

	/**
	 * Add attributes, literals or methods depending on the given Java type
	 * @param parentItem
	 */
	private void addJavaTypeItems(TreeItem parentItem) {
		try {
			// Remove the dummy item
			parentItem.removeAll();

			final JavaType t = (JavaType) parentItem.getData();

			if (t instanceof final DomainObject domainObject)
				addDomainObjectChildItems(parentItem, domainObject);
			else if (t instanceof final JavaEnum javaEnum)
				addJavaEnumChildItems(parentItem, javaEnum);
			else if (t instanceof final Repository repository)
				addRepositoryChildItems(parentItem, repository);
			else if (t instanceof final DTOBean dto)
				addDTOChildItems(parentItem, dto);
			else if (t instanceof final ExchangeMappingObject exchangeMappingObject)
				addMappingObjectChildItems(parentItem, exchangeMappingObject);
			else if (t instanceof final BoundaryBean boundaryBean)
				addBoundaryChildItems(parentItem, boundaryBean);
			else if (t instanceof final DataExchangeServiceBean exchangeBean)
				addDataExchangeServiceChildItems(parentItem, exchangeBean);
			else if (t instanceof final AbstractIntegrationBean integrationBean)
				addIntegrationBeanChildItems(parentItem, integrationBean);
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
		}
	}

	/**
	 * Add all methods of the boundary bean
	 * @param parentItem
	 * @param boundaryBean
	 */
	public void addBoundaryChildItems(TreeItem parentItem, BoundaryBean boundaryBean) {
		// Remove the dummy item
		parentItem.removeAll();

		final Project project = boundaryBean.getNamespace().getProject();
		final var boundaryService = new BoundaryService(project);
		final var repositoryService = new RepositoryService(project);

		if (project.isBoundaryMode()) {
			boundaryBean.getBoundaryMethods().forEach(method -> {
				final var item = new TreeItem(parentItem, SWT.NONE);
				item.setData(method);
				item.setText(boundaryService.getBoundaryMethodSignature(method));
				item.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PUBLIC));
			});
		}
		else {
			for (final BoundaryMethod method : boundaryBean.getBoundaryMethods()) {
				final String methodSignature = boundaryService.getFacadeMethodSignature(method);

				if (methodSignature == null)
					continue;

				final var item = new TreeItem(parentItem, SWT.NONE);
				item.setData(method);
				item.setText(methodSignature);
				item.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PUBLIC));
			}

			// Add all methods of the respective repository
			for (final RepositoryMethod method : boundaryBean.getRepository().getRepositoryMethods()) {
				if (method.getMethodType() == RepositoryMethodTypeEnumeration.DELETE)
					continue;

				if (method.getMethodType() == RepositoryMethodTypeEnumeration.GET_ASSOCIATION)
					continue;

				final var item = new TreeItem(parentItem, SWT.NONE);
				item.setData(method);
				item.setText(repositoryService.getMethodSignature(method));
				item.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PUBLIC));
			}
		}
	}

	/**
	 * Add all methods of the data exchange service
	 * @param parentItem
	 * @param dataExchangeBean
	 */
	public void addDataExchangeServiceChildItems(TreeItem parentItem, DataExchangeServiceBean dataExchangeBean) {
		// Remove the dummy item
		parentItem.removeAll();

		final var dataExchangeBeanService = new DataExchangeBeanService(dataExchangeBean.getNamespace().getProject());

		dataExchangeBean.getDataExchangeMethods().forEach(method -> {
			final var item = new TreeItem(parentItem, SWT.NONE);
			item.setData(method);
			item.setText(dataExchangeBeanService.getMethodSignature(method));
			item.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PUBLIC));
		});
	}

	/**
	 * Add all methods of a repository
	 * @param parentItem
	 * @param repository
	 */
	public void addRepositoryChildItems(TreeItem parentItem, Repository repository) {
		// Remove the dummy item
		parentItem.removeAll();

		final var repositoryService = new RepositoryService(repository.getNamespace().getProject());

		repository.getRepositoryMethods().forEach(method -> {
			final var item = new TreeItem(parentItem, SWT.NONE);
			item.setData(method);
			item.setText(repositoryService.getMethodSignature(method));
			item.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PUBLIC));
		});
	}

	/**
	 * Add all literals of a Java enum
	 * @param parentItem
	 * @param javaEnum
	 */
	public void addJavaEnumChildItems(TreeItem parentItem, JavaEnum javaEnum) {
		// Remove the dummy item
		parentItem.removeAll();

		javaEnum.getEnumerationValues().forEach(literal -> {
			final var item = new TreeItem(parentItem, SWT.NONE);
			item.setText(literal.getName());
			item.setData(literal);
			item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ENUM_LITERAL));
		});
	}

	/**
	 * Add all attributes of a domain object
	 * @param parentItem
	 * @param domainObj
	 */
	public void addDomainObjectChildItems(TreeItem parentItem, DomainObject domainObj) {
		// Remove the dummy item
		parentItem.removeAll();

		domainObj.getAttributes().forEach(attr -> {
			final var item = new TreeItem(parentItem, SWT.NONE);
			item.setText(attr.getTypeName() + " " + attr.getName());
			item.setData(attr);
			item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));

			if (attr.getDomainAttributeValidator().isNullable())
				item.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));
		});

		domainObj.getAssociations().forEach(assoc -> {
			final var item = new TreeItem(parentItem, SWT.NONE);
			item.setData(assoc);

			if (assoc instanceof ManyToManyAssociation) {
				item.setText(
						JavaTypeModifierEnumeration.COLLECTION.toString() + "<" + assoc.getTarget().getName() + "> " + assoc.getName());
				item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_MTM_ASSOC));
			}
			else if (assoc instanceof final ManyToOneAssociation mto) {
				item.setText(assoc.getTarget().getName() + " " + assoc.getName());
				item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_MTO_ASSOC));

				if (mto.isOptional())
					item.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));
			}
			else if (assoc instanceof OneToManyAssociation) {
				item.setText(
						JavaTypeModifierEnumeration.COLLECTION.toString() + "<" + assoc.getTarget().getName() + "> " + assoc.getName());
				item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_OTM_ASSOC));
			}
			else if (assoc instanceof final OneToOneAssociation oto) {
				item.setText(assoc.getTarget().getName() + " " + assoc.getName());
				item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_OTO_ASSOC));

				if (oto.isOptional())
					item.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_GRAY));
			}
		});
	}

	/**
	 * Add all attributes of a DTO bean
	 * @param parentItem
	 * @param dto
	 */
	public void addDTOChildItems(TreeItem parentItem, DTOBean dto) {
		// Remove the dummy item
		parentItem.removeAll();

		dto.getAttributes().forEach(attr -> {
			final var item = new TreeItem(parentItem, SWT.NONE);
			item.setData(attr);
			item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));

			if (attr.getReferencedDTOBean() != null && attr.getAssociation() != null) {
				if (attr.getAssociation() instanceof OneToOneAssociation || attr.getAssociation() instanceof ManyToOneAssociation)
					item.setText(attr.getReferencedDTOBean().getName() + " " + attr.getName());
				else
					item.setText(JavaTypeModifierEnumeration.COLLECTION.toString() + "<" + attr.getReferencedDTOBean().getName() + "> "
							+ attr.getName());
			}
			else if (attr.getDomainAttribute() != null)
				item.setText(attr.getDomainAttribute().getTypeName() + " " + attr.getName());
		});
	}

	/**
	 * Add all attributes of an exchange mapping object
	 * @param parentItem
	 * @param mappingObj
	 */
	public void addMappingObjectChildItems(TreeItem parentItem, ExchangeMappingObject mappingObj) {
		// Remove the dummy item
		parentItem.removeAll();

		mappingObj.getAttributes().forEach(attr -> {
			String typeName = attr.getJavaType().getName();

			if (attr.getDomainAttribute() != null)
				typeName = attr.getDomainAttribute().getTypeName();

			// By definition, we handle fields of type char by using a String with exactly one character!
			if (typeName.equals(JavaType.CHAR))
				typeName = JavaType.STRING;

			final var item = new TreeItem(parentItem, SWT.NONE);
			item.setData(attr);
			item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));

			if (attr.getModifier() == JavaTypeModifierEnumeration.NONE)
				item.setText(typeName + " " + attr.getName());
			else
				item.setText(attr.getModifier().toString() + "<" + typeName + "> " + attr.getName());
		});
	}

	/**
	 * Add all methods of an integration bean
	 * @param parentItem
	 * @param integrationBean
	 */
	public void addIntegrationBeanChildItems(TreeItem parentItem, AbstractIntegrationBean integrationBean) {
		// Remove the dummy item
		parentItem.removeAll();

		final var integrationBeanService = new IntegrationBeanService(integrationBean.getNamespace().getProject());

		integrationBean.getMethods().forEach(method -> {
			final var item = new TreeItem(parentItem, SWT.NONE);
			item.setData(method);
			item.setText(integrationBeanService.getMethodSignature(method));
			item.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PUBLIC));
		});
	}

	/**
	 * Add form groups
	 * @param parentItem
	 */
	private void addFormGroups(TreeItem parentItem) {
		FormGroup parentGroup = null;
		EList<FormGroup> groups = null;

		if (parentItem.getData() == null && parentItem.getText().equals(FORM_GROUPS_LABEL)) {
			final var project = (Project) parentItem.getParentItem().getData();
			groups = project.getFormGroups();
		}
		else {
			parentGroup = (FormGroup) parentItem.getData();
			groups = parentGroup.getChildGroups();
		}

		// Sort the form groups
		ECollections.sort(groups, new FormGroupComparator());

		groups.forEach(group -> {
			final var groupItem = new TreeItem(parentItem, SWT.NONE);
			groupItem.setText(group.getName());
			groupItem.setData(group);
			groupItem.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

			addFormGroups(groupItem);

			// Add the forms of this group
			final EList<Form> forms = group.getForms();
			ECollections.sort(forms, new FormComparator());

			forms.forEach(form -> {
				final var formItem = new TreeItem(groupItem, SWT.NONE);
				formItem.setText(form.getName());
				formItem.setData(form);

				if (form.getFormType() == FormTypeEnumeration.CREATE)
					formItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_CREATE));
				else if (form.getFormType() == FormTypeEnumeration.UPDATE)
					formItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_EDIT));
				else if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW
						|| form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW)
					formItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_VIEW));
				else if (form.getFormType() == FormTypeEnumeration.READONLY)
					formItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_READ));
				else if (form.getFormType() == FormTypeEnumeration.ADD)
					formItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_ADD));
				else if (form.getFormType() == FormTypeEnumeration.LOV)
					formItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_LOV));
				else if (form.getFormType() == FormTypeEnumeration.TREE_VIEW)
					formItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_VIEW));
			});

			// Add the grid panels of this group
			if (group.getPanels() != null) {
				final EList<FormPanel> panels = group.getPanels();
				ECollections.sort(panels, new PanelComparator());

				panels.forEach(panel -> {
					final var formItem = new TreeItem(groupItem, SWT.NONE);
					formItem.setText(panel.getName());
					formItem.setData(panel);
					formItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_PANEL));
				});
			}
		});
	}

	/**
	 * Add the top-level items of a project
	 * @param projectItem the parent project tree item
	 */
	public void addProjectItems(TreeItem projectItem) {
		try {
			final var project = (Project) projectItem.getData();

			// Remove the dummy item
			projectItem.removeAll();

			// Add a container item for all domain object namespace items
			final var puItem = new TreeItem(projectItem, SWT.NONE);
			puItem.setText(DOMAIN_NAMESPACE_LABEL);
			puItem.setData(project.getDomainNamespace());
			puItem.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

			// Add the domain object namespace items
			EList<Namespace> namespaces = project.getDomainNamespace().getChildNamespaces();
			ECollections.sort(namespaces, new NamespaceComparator());

			namespaces.forEach(ns -> {
				final var contextItem = new TreeItem(puItem, SWT.NONE);
				contextItem.setText(ns.getName());
				contextItem.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PACKAGE));
				contextItem.setData(ns);

				ProjectTreeViewHelper.addNamespace(ns.toString());

				// Add a dummy item
				new TreeItem(contextItem, SWT.NONE);
			});

			if (project.isBoundaryMode()) {
				final var repositoryItem = new TreeItem(projectItem, SWT.NONE);
				repositoryItem.setText(REPOSITORY_NAMESPACE_LABEL);
				repositoryItem.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
				repositoryItem.setData(project.getRepositoryNamespace());

				ProjectTreeViewHelper.setNamespaceLoaded(project.getRepositoryNamespace().toString());

				// Add the repository namespace items
				namespaces = project.getRepositoryNamespace().getChildNamespaces();
				ECollections.sort(namespaces, new NamespaceComparator());

				namespaces.forEach(ns -> {
					final var contextItem = new TreeItem(repositoryItem, SWT.NONE);
					contextItem.setText(ns.getName());
					contextItem.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PACKAGE));
					contextItem.setData(ns);

					ProjectTreeViewHelper.addNamespace(ns.toString());

					new TreeItem(contextItem, SWT.NONE);
				});
			}

			final var dtoItem = new TreeItem(projectItem, SWT.NONE);
			dtoItem.setText(DTO_NAMESPACE_LABEL);
			dtoItem.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
			dtoItem.setData(project.getDTONamespace());

			ProjectTreeViewHelper.setNamespaceLoaded(project.getDTONamespace().toString());

			// Add the DTO namespace items
			namespaces = project.getDTONamespace().getChildNamespaces();
			ECollections.sort(namespaces, new NamespaceComparator());

			namespaces.forEach(ns -> {
				final var contextItem = new TreeItem(dtoItem, SWT.NONE);
				contextItem.setText(ns.getName());
				contextItem.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PACKAGE));
				contextItem.setData(ns);

				ProjectTreeViewHelper.addNamespace(ns.toString());

				// Add a dummy item
				new TreeItem(contextItem, SWT.NONE);
			});

			if (!project.getIntegrationModules().isEmpty()) {
				final var integrationItem = new TreeItem(projectItem, SWT.NONE);
				integrationItem.setText(INTEGRATION_LABEL);
				integrationItem.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

				for (final IntegrationModule module : project.getIntegrationModules()) {
					final var moduleItem = new TreeItem(integrationItem, SWT.NONE);
					moduleItem.setText(module.getNamespace().getName());
					moduleItem.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PACKAGE));
					moduleItem.setData(module.getNamespace());

					ProjectTreeViewHelper.addNamespace(module.getNamespace().toString());

					// Add a dummy item
					new TreeItem(moduleItem, SWT.NONE);
				}
			}

			// Add the test module items
			if (!project.getTestModules().isEmpty()) {
				final var testModuleRootItem = new TreeItem(projectItem, SWT.NONE);
				testModuleRootItem.setText(TEST_MODULES_LABEL);
				testModuleRootItem.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

				project.getTestModules().forEach(module -> {
					String moduleName = TEST_MODULE_NAME_UNDEF;

					if (module instanceof SeleniumTestModule)
						moduleName = TEST_MODULE_NAME_SELENIUM;
					else if (module instanceof final IntegrationTestModule integrationTestModule)
						moduleName = integrationTestModule.getIntegrationModule().getTechnology().getName();

					final var moduleItem = new TreeItem(testModuleRootItem, SWT.NONE);
					moduleItem.setText(moduleName);
					moduleItem.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
					moduleItem.setData(module.getNamespace());

					ProjectTreeViewHelper.addNamespace(module.getNamespace().toString());

					// Add a dummy item
					new TreeItem(moduleItem, SWT.NONE);
				});
			}

			final var exchangeItem = new TreeItem(projectItem, SWT.NONE);
			exchangeItem.setText(DATA_EXCHANGE_NAMESPACE_LABEL);
			exchangeItem.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
			exchangeItem.setData(project.getExchangeNamespace());

			ProjectTreeViewHelper.setNamespaceLoaded(project.getExchangeNamespace().toString());

			// Add the data exchange namespace items
			namespaces = project.getExchangeNamespace().getChildNamespaces();
			ECollections.sort(namespaces, new NamespaceComparator());

			namespaces.forEach(ns -> {
				final var contextItem = new TreeItem(exchangeItem, SWT.NONE);
				contextItem.setText(ns.getName());
				contextItem.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PACKAGE));
				contextItem.setData(ns);

				ProjectTreeViewHelper.addNamespace(ns.toString());

				// Add a dummy item
				new TreeItem(contextItem, SWT.NONE);
			});

			final var boundaryItem = new TreeItem(projectItem, SWT.NONE);

			if (project.isBoundaryMode())
				boundaryItem.setText(BOUNDARY_NAMESPACE_LABEL);
			else
				boundaryItem.setText(FACADE_NAMESPACE_LABEL);

			boundaryItem.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
			boundaryItem.setData(project.getBoundaryNamespace());

			ProjectTreeViewHelper.setNamespaceLoaded(project.getBoundaryNamespace().toString());

			// Add the boundary namespace items
			namespaces = project.getBoundaryNamespace().getChildNamespaces();
			ECollections.sort(namespaces, new NamespaceComparator());

			namespaces.forEach(ns -> {
				final var contextItem = new TreeItem(boundaryItem, SWT.NONE);
				contextItem.setText(ns.getName());
				contextItem.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PACKAGE));
				contextItem.setData(ns);

				ProjectTreeViewHelper.addNamespace(ns.toString());

				// Add a dummy item
				new TreeItem(contextItem, SWT.NONE);
			});

			final var dbItem = new TreeItem(projectItem, SWT.NONE);
			dbItem.setText(DATABASE_LABEL);
			dbItem.setData(project.getDatabase());
			dbItem.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

			// Add a dummy item
			new TreeItem(dbItem, SWT.NONE);

			ProjectTreeViewHelper.addDatabase(project.getDatabase().hashCode());

			if (project.hasClient()) {
				final var formGroupsItem = new TreeItem(projectItem, SWT.NONE);
				formGroupsItem.setText(FORM_GROUPS_LABEL);
				formGroupsItem.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

				// Add the top-level form groups
				addFormGroups(formGroupsItem);
			}
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
		}
	}

	/**
	 * Build the project tree view
	 */
	private void buildTree() {
		ProjectTreeViewHelper.reset();

		// Get all projects and add them to the tree view
		final IWorkspaceRoot wsRoot = CodeCadenzaUserInterfacePlugin.getWorkspace().getRoot();

		for (final IProject project : wsRoot.getProjects()) {
			if (!project.isAccessible())
				continue;

			final var f = new File(
					project.getLocation().toString() + File.separatorChar + MODEL_FOLDER + File.separatorChar + MODEL_ROOT_FILE);

			if (f.exists()) {
				final var resourceSet = new ResourceSetImpl();
				final URI namespaceURI = URI.createFileURI(f.getAbsolutePath());

				// Get the resource object
				final Resource projectResource = resourceSet.getResource(namespaceURI, true);

				for (final EObject e : projectResource.getContents()) {
					if (!(e instanceof final Project proj))
						continue;

					final var projectItem = new TreeItem(tree, SWT.NONE);
					projectItem.setText(proj.getName());
					projectItem.setData(proj);
					projectItem.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(IDE.SharedImages.IMG_OBJ_PROJECT));

					ProjectTreeViewHelper.addProject(proj.hashCode());

					if (currentSelection != null && currentSelection.belongsToProject(proj)) {
						// Try to fully expand the tree to the last open item!
						currentSelection.expandTree(projectItem);
					}
					else
						new TreeItem(projectItem, SWT.NONE);
				}
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createPartControl(Composite parent) {
		final var toolkit = new FormToolkit(Display.getCurrent());

		final Composite panViewArea = toolkit.createComposite(parent, SWT.NONE);
		panViewArea.setLayout(new FillLayout());

		toolkit.paintBordersFor(panViewArea);

		thisView = this;
		shell = this.getSite().getShell();

		// Register a listener to get notified about events of the graphical domain editor
		GraphicalEditorEventController.addListener(thisView);

		tree = new Tree(panViewArea, SWT.NONE);

		tree.addMenuDetectListener(_ -> {
			TreeItem selItem = null;

			if (tree.getMenu() != null)
				tree.getMenu().dispose();

			tree.setMenu(null);

			final TreeItem[] selItems = tree.getSelection();

			for (final TreeItem item : selItems)
				selItem = item;

			if (selItem == null)
				return;

			// Determine the kind of the connected data object in order to set the proper menu
			if (selItem.getData() instanceof Project)
				tree.setMenu(new ProjectMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof final RepositoryMethod method) {
				final Project project = method.getRepository().getNamespace().getProject();

				tree.setMenu(new RepositoryMethodMenuBuilder(thisView, tree, project.isBoundaryMode()).createMenu());
			}
			else if (selItem.getData() instanceof Repository)
				tree.setMenu(new RepositoryMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof BoundaryBean)
				tree.setMenu(new BoundaryBeanMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof Database)
				tree.setMenu(new DatabaseMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof JavaEnum)
				tree.setMenu(new JavaEnumMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof DomainObject)
				tree.setMenu(new DomainObjectMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof DTOBean)
				tree.setMenu(new DTOBeanMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof DBTable)
				tree.setMenu(new TableMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof BoundaryMethod)
				tree.setMenu(new BoundaryMethodMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof FormGroup)
				tree.setMenu(new FormGroupMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof DataExchangeServiceBean)
				tree.setMenu(new DataExchangeServiceBeanMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof DataExchangeMethod)
				tree.setMenu(new DataExchangeMethodMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof ExchangeMappingObject)
				tree.setMenu(new ExchangeMappingObjectMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof Form)
				tree.setMenu(new FormMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof FormPanel)
				tree.setMenu(new PanelMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getText().equals(DTO_NAMESPACE_LABEL))
				tree.setMenu(new DTONamespaceMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getText().equals(DATA_EXCHANGE_NAMESPACE_LABEL))
				tree.setMenu(new ExchangeNamespaceMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getText().equals(REPOSITORY_NAMESPACE_LABEL))
				tree.setMenu(new RepositoryNamespaceMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getText().equals(BOUNDARY_NAMESPACE_LABEL))
				tree.setMenu(new BoundaryNamespaceMenuBuilder(thisView, tree, true).createMenu());
			else if (selItem.getText().equals(FACADE_NAMESPACE_LABEL))
				tree.setMenu(new BoundaryNamespaceMenuBuilder(thisView, tree, false).createMenu());
			else if (selItem.getText().equals(FORM_GROUPS_LABEL))
				tree.setMenu(new FormGroupFolderMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof Namespace && selItem.getParentItem().getText().equals(INTEGRATION_LABEL))
				tree.setMenu(new IntegrationNamespaceMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof DomainNamespace && selItem.getParentItem().getData() instanceof Namespace)
				tree.setMenu(new DomainNamespaceMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof Namespace && selItem.getParentItem().getData() instanceof Namespace)
				tree.setMenu(new ChildNamespaceMenuBuilder<>(thisView, tree).createMenu());
			else if (selItem.getData() instanceof AbstractIntegrationBean)
				tree.setMenu(new IntegrationBeanMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof AbstractIntegrationMethod)
				tree.setMenu(new IntegrationMethodMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof GUITestCase)
				tree.setMenu(new GUITestCaseMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof IntegrationTestCase)
				tree.setMenu(new IntegrationTestCaseMenuBuilder(thisView, tree).createMenu());
			else if (selItem.getData() instanceof final Namespace testModuleNamespace
					&& selItem.getParentItem().getText().equals(TEST_MODULES_LABEL)) {
				final Project project = testModuleNamespace.getProject();

				for (final AbstractTestModule testModule : project.getTestModules())
					if (testModule.getNamespace().equals(testModuleNamespace))
						if (testModule instanceof IntegrationTestModule)
							tree.setMenu(new IntegrationTestNamespaceMenuBuilder(thisView, tree).createMenu());
						else
							tree.setMenu(new GUITestModuleNamespaceMenuBuilder(thisView, tree).createMenu());
			}
			else if (selItem.getData() instanceof TestSuite)
				tree.setMenu(new TestSuiteMenuBuilder(thisView, tree).createMenu());
		});

		tree.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseAdapter#mouseDown(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseDown(MouseEvent e) {
				TreeItem selItem = null;
				final TreeItem[] selItems = tree.getSelection();

				for (final TreeItem item : selItems)
					selItem = item;

				if (selItem == null)
					return;

				currentSelection = null;

				if (selItem.getData() instanceof final JavaType javaType)
					currentSelection = new TreeSelectionHelper(ProjectView.this, javaType);
				else if (selItem.getData() instanceof final Form form)
					currentSelection = new TreeSelectionHelper(ProjectView.this, form);
				else if (selItem.getData() instanceof final FormPanel formPanel)
					currentSelection = new TreeSelectionHelper(ProjectView.this, formPanel);
				else if (selItem.getData() instanceof final FormGroup formGroup)
					currentSelection = new TreeSelectionHelper(ProjectView.this, formGroup);
				else if (selItem.getData() instanceof final BoundaryMethod boundaryMethod)
					currentSelection = new TreeSelectionHelper(ProjectView.this, boundaryMethod);
				else if (selItem.getData() instanceof final DataExchangeMethod exchangeMethod)
					currentSelection = new TreeSelectionHelper(ProjectView.this, exchangeMethod);
				else if (selItem.getData() instanceof final AbstractIntegrationMethod integrationMethod)
					currentSelection = new TreeSelectionHelper(ProjectView.this, integrationMethod);
				else if (selItem.getData() instanceof final Namespace namespace)
					currentSelection = new TreeSelectionHelper(ProjectView.this, namespace);
				else if (selItem.getData() instanceof final DomainAttribute domainAttribute)
					currentSelection = new TreeSelectionHelper(ProjectView.this, domainAttribute);
				else if (selItem.getData() instanceof final EnumLiteral enumLiteral)
					currentSelection = new TreeSelectionHelper(ProjectView.this, enumLiteral);
				else if (selItem.getData() instanceof final DTOBeanAttribute dtoAttribute)
					currentSelection = new TreeSelectionHelper(ProjectView.this, dtoAttribute);
				else if (selItem.getData() instanceof final Database database)
					currentSelection = new TreeSelectionHelper(ProjectView.this, database);
				else if (selItem.getData() instanceof final DBTable table)
					currentSelection = new TreeSelectionHelper(ProjectView.this, table);
				else if (selItem.getData() instanceof final AbstractDomainAssociation domainAssociation)
					currentSelection = new TreeSelectionHelper(ProjectView.this, domainAssociation);
				else if (selItem.getData() instanceof final DBColumn column)
					currentSelection = new TreeSelectionHelper(ProjectView.this, column);
				else if (selItem.getData() instanceof final Project project)
					currentSelection = new TreeSelectionHelper(ProjectView.this, project, selItem.getText());
				else if (selItem.getData() instanceof final RepositoryMethod method) {
					final Project project = method.getRepository().getNamespace().getProject();

					if (!project.isBoundaryMode()) {
						final var boundary = (BoundaryBean) selItem.getParentItem().getData();
						currentSelection = new TreeSelectionHelper(ProjectView.this, boundary, method);
					}
					else
						currentSelection = new TreeSelectionHelper(ProjectView.this, method);
				}

				if (selItem.getData() == null)
					if (selItem.getText().equals(FORM_GROUPS_LABEL))
						currentSelection = new TreeSelectionHelper(ProjectView.this, (Project) selItem.getParentItem().getData(),
								FORM_GROUPS_LABEL);
					else if (selItem.getText().equals(INTEGRATION_LABEL))
						currentSelection = new TreeSelectionHelper(ProjectView.this, (Project) selItem.getParentItem().getData(),
								INTEGRATION_LABEL);
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseAdapter#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				TreeItem selItem = null;

				if (e.button != 1)
					return;

				final TreeItem[] selItems = tree.getSelection();

				for (final TreeItem item : selItems)
					selItem = item;

				if (selItem == null)
					return;

				if (selItem.getData() instanceof JavaEnum)
					openEnumInEditor(selItem);
				else if (selItem.getData() instanceof Repository)
					openRepositoryInEditor(selItem);
				else if (selItem.getData() instanceof DomainNamespace && selItem.getParentItem().getData() instanceof Namespace)
					openDomainNamespaceInEditor(selItem);
				else if (selItem.getData() instanceof DomainObject)
					openDomainObjectInEditor(selItem);
				else if (selItem.getData() instanceof ExchangeMappingObject)
					openExchangeMappingObjectInEditor(selItem);
				else if (selItem.getData() instanceof DTOBean)
					openDTOBeanInEditor(selItem);
				else if (selItem.getData() instanceof BoundaryBean)
					openBoundaryInEditor(selItem);
				else if (selItem.getData() instanceof Form)
					openFormInEditor(selItem);
				else if (selItem.getData() instanceof FormPanel)
					openGridPanelInEditor(selItem);
				else if (selItem.getData() instanceof DataExchangeServiceBean)
					openExchangeServiceBeanInEditor(selItem);
				else if (selItem.getData() instanceof DataExchangeMethod)
					openExchangeMethodInDialog(selItem);
				else if (selItem.getData() instanceof AbstractIntegrationBean)
					openIntegrationBeanInEditor(selItem);
				else if (selItem.getData() instanceof AbstractIntegrationMethod)
					openIntegrationMethodInDialog(selItem);
				else if (selItem.getData() instanceof GUITestCase)
					openGUITestCaseInEditor(selItem);
				else if (selItem.getData() instanceof IntegrationTestCase)
					openIntegrationTestCaseInEditor(selItem);
				else if (selItem.getData() instanceof TestSuite)
					openTestSuiteInEditor(selItem);
			}
		});

		toolkit.adapt(tree, true, true);
		tree.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		tree.addTreeListener(new TreeAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.TreeAdapter#treeExpanded(org.eclipse.swt.events.TreeEvent)
			 */
			@Override
			public void treeExpanded(TreeEvent e) {
				try {
					getViewSite().getShell().setCursor(getViewSite().getShell().getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
					final var item = (TreeItem) e.item;

					// Check if the project has been already loaded
					if (item.getData() instanceof final Project project && !ProjectTreeViewHelper.isProjectLoaded(project.hashCode())) {
						// Add the top-level items of a project
						addProjectItems(item);

						// Determine that top-level items of a project have been added
						ProjectTreeViewHelper.setProjectLoaded(project.hashCode());
					}
					else if (item.getData() instanceof final Namespace namespace) {
						final String name = namespace.toString();

						if (!ProjectTreeViewHelper.isNamespaceLoaded(name)) {
							// Add the sub-items of a namespace item
							addNamespaceItems(item);

							// Determine that the namespace sub-items have been added
							ProjectTreeViewHelper.setNamespaceLoaded(name);
						}
					}
					else if (item.getData() instanceof final Database database && !ProjectTreeViewHelper.isDBLoaded(database.hashCode())) {
						// Add the sub-items of a database item
						addDatabaseItems(item);

						// Determine that the database sub-items have been added
						ProjectTreeViewHelper.setDBLoaded(database.hashCode());
					}
					else if (item.getData() instanceof final DBTable dbTable && !ProjectTreeViewHelper.isTableLoaded(dbTable.hashCode())) {
						// Add the sub-items of a table item
						addTableItems(item);

						// Determine that the table sub-items have been added
						ProjectTreeViewHelper.setTableLoaded(dbTable.hashCode());
					}
					else if (item.getData() instanceof final JavaType javaType
							&& !ProjectTreeViewHelper.isJavaTypeLoaded(javaType.hashCode())) {
						// Add the sub-items to a Java type item
						addJavaTypeItems(item);

						// Determine that the Java type sub-items have been added
						ProjectTreeViewHelper.setJavaTypeLoaded(javaType.hashCode());
					}
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().logError(ex);
				}
				finally {
					getViewSite().getShell().setCursor(getViewSite().getShell().getDisplay().getSystemCursor(SWT.CURSOR_ARROW));
				}
			}
		});

		// Allow data to be copied or moved to the drop target
		final var target = new DropTarget(tree, DND.DROP_MOVE | DND.DROP_COPY | DND.DROP_DEFAULT);

		// Receive the data in text format
		target.setTransfer(TextTransfer.getInstance());

		target.addDropListener(new DropTargetListener() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetListener#dragEnter(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dragEnter(DropTargetEvent event) {
				event.detail = DND.DROP_COPY;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetListener#dragOver(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dragOver(DropTargetEvent event) {
				// No implementation required!
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetListener#dragOperationChanged(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dragOperationChanged(DropTargetEvent event) {
				// No implementation required!
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetListener#dragLeave(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dragLeave(DropTargetEvent event) {
				// No implementation required!
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetListener#dropAccept(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dropAccept(DropTargetEvent event) {
				final TreeItem itemDrop = tree.getItem(tree.toControl(new Point(event.x, event.y)));

				if (itemDrop == null) {
					event.detail = DND.DROP_NONE;
					return;
				}

				if (itemDrop.getData() instanceof FormGroup) {
					event.detail = DND.DROP_COPY;
					return;
				}

				event.detail = DND.DROP_NONE;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetListener#drop(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void drop(DropTargetEvent event) {
				try {
					final TreeItem itemDrop = tree.getItem(tree.toControl(new Point(event.x, event.y)));

					if (itemDrop == null)
						return;

					if (!(itemDrop.getData() instanceof final FormGroup group))
						return;

					TreeItem dragItem = null;
					final TreeItem[] selItems = tree.getSelection();

					for (final TreeItem item : selItems)
						dragItem = item;

					if (dragItem == null)
						return;

					final var options = new HashMap<String, String>();
					options.put(XMLResource.OPTION_PROCESS_DANGLING_HREF, "DISCARD");

					if (dragItem.getData() instanceof final Form form) {
						form.setFormGroup(group);

						final var formItem = new TreeItem(itemDrop, SWT.NONE);
						formItem.setText(form.getName());
						formItem.setData(form);

						if (form.getFormType() == FormTypeEnumeration.CREATE)
							formItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_CREATE));
						else if (form.getFormType() == FormTypeEnumeration.UPDATE)
							formItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_EDIT));
						else if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW
								|| form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW)
							formItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_VIEW));
						else if (form.getFormType() == FormTypeEnumeration.READONLY)
							formItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_READ));
						else if (form.getFormType() == FormTypeEnumeration.ADD)
							formItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_ADD));
						else if (form.getFormType() == FormTypeEnumeration.LOV)
							formItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_LOV));
						else if (form.getFormType() == FormTypeEnumeration.TREE_VIEW)
							formItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_VIEW));
					}
					else if (dragItem.getData() instanceof final FormPanel panel) {
						panel.setFormGroup(group);

						final var formItem = new TreeItem(itemDrop, SWT.NONE);
						formItem.setText(panel.getName());
						formItem.setData(panel);
						formItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_PANEL));
					}

					dragItem.dispose();
					group.eResource().save(options);
				}
				catch (final Exception e) {
					CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
				}
			}
		});

		// Allow data to be copied or moved from the drag source
		final var source = new DragSource(tree, DND.DROP_MOVE | DND.DROP_COPY);

		// Provide the data in text format
		source.setTransfer(TextTransfer.getInstance());

		source.addDragListener(new DragSourceListener() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DragSourceListener#dragStart(org.eclipse.swt.dnd.DragSourceEvent)
			 */
			@Override
			public void dragStart(DragSourceEvent event) {
				TreeItem selItem = null;
				final TreeItem[] selItems = tree.getSelection();

				for (final TreeItem item : selItems)
					selItem = item;

				if (selItem == null) {
					event.doit = false;
					return;
				}

				if (selItem.getData() instanceof Form || selItem.getData() instanceof FormPanel) {
					event.doit = true;
					return;
				}

				event.doit = false;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DragSourceListener#dragSetData(org.eclipse.swt.dnd.DragSourceEvent)
			 */
			@Override
			public void dragSetData(DragSourceEvent event) {
				TreeItem selItem = null;
				final TreeItem[] selItems = tree.getSelection();

				for (final TreeItem item : selItems)
					selItem = item;

				if (selItem == null)
					return;

				if (selItem.getData() instanceof Form || selItem.getData() instanceof FormPanel) {
					// We set the drag data but we do not care for it when it comes to a drop!
					event.data = selItem.getData().toString();
				}
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DragSourceListener#dragFinished(org.eclipse.swt.dnd.DragSourceEvent)
			 */
			@Override
			public void dragFinished(DragSourceEvent event) {
				// No implementation required!
			}
		});

		// Build the tree view
		try {
			buildTree();
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
			return;
		}

		createActions();
		initializeToolBar();

		ResourcesPlugin.getWorkspace().addResourceChangeListener(this);
	}

	/**
	 * Create the actions
	 */
	private void createActions() {
		refreshAction = new RefreshAction();
	}

	/**
	 * Initialize the toolbar
	 */
	private void initializeToolBar() {
		final IToolBarManager tbm = getViewSite().getActionBars().getToolBarManager();
		tbm.add(refreshAction);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
	 */
	@Override
	public void setFocus() {
		// No implementation required!
	}

	/**
	 * Open the dialog to edit the selected data exchange method in the project view
	 * @param selItem
	 */
	public void openExchangeMethodInDialog(TreeItem selItem) {
		if (EclipseIDEService.openDiagramExists(this))
			return;

		if (selItem.getData() == null)
			return;

		if (!(selItem.getData() instanceof final DataExchangeMethod exchangeMethod))
			return;

		final Project project = exchangeMethod.getDataExchangeServiceBean().getNamespace().getProject();
		final var dataExchangeBeanService = new DataExchangeBeanService(project);

		// Open the dialog
		final var dlg = new EditExchangeMethodDialog(shell, exchangeMethod);

		if (dlg.open() == Dialog.OK) {
			// Save the changes
			try {
				selItem.setText(dataExchangeBeanService.getMethodSignature(exchangeMethod));

				EclipseIDEService.saveProjectMetaData(project);
			}
			catch (final Exception ex) {
				CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
			}
		}

		// Refresh the tree no matter if the dialog has been closed with 'Cancel' or 'OK' in order to revert or to sync changes!
		refreshTree();
	}

	/**
	 * Open the dialog to edit the selected integration method in the project view
	 * @param selItem
	 */
	public void openIntegrationMethodInDialog(TreeItem selItem) {
		if (EclipseIDEService.openDiagramExists(this))
			return;

		if (selItem.getData() == null)
			return;

		if (!(selItem.getData() instanceof final AbstractIntegrationMethod integrationMethod))
			return;

		final Project project = integrationMethod.getIntegrationBean().getNamespace().getProject();
		final var integrationBeanService = new IntegrationBeanService(project);

		// Open the dialog
		final var dlg = new EditIntegrationMethodDialog(shell, integrationMethod);

		if (dlg.open() == Dialog.OK) {
			// Save the changes
			try {
				selItem.setText(integrationBeanService.getMethodSignature(integrationMethod));

				EclipseIDEService.saveProjectMetaData(project);
			}
			catch (final Exception ex) {
				CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
			}
		}

		// Refresh the tree no matter if the dialog has been closed with 'Cancel' or 'OK' in order to revert or to sync changes!
		refreshTree();
	}

	/**
	 * Open the selected domain namespace in the diagram editor
	 * @param item
	 */
	public void openDomainNamespaceInEditor(TreeItem item) {
		final var domainNamespace = (DomainNamespace) item.getData();

		try {
			final IEditorPart editor = EclipseIDEService.openInEditor(domainNamespace.getDiagramFile());

			if (editor != null)
				editor.addPropertyListener((_, _) -> refreshTree());
		}
		catch (final Exception ex) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
		}
	}

	/**
	 * Open the selected enumeration in the editor
	 * @param item
	 */
	public void openEnumInEditor(TreeItem item) {
		final var javaEnum = (JavaEnum) item.getData();

		try {
			EclipseIDEService.openInEditor(javaEnum.getSourceFile());
		}
		catch (final Exception ex) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
		}
	}

	/**
	 * Open the selected domain object in the editor
	 * @param item
	 */
	public void openDomainObjectInEditor(TreeItem item) {
		final var domainObject = (DomainObject) item.getData();

		try {
			EclipseIDEService.openInEditor(domainObject.getSourceFile());
		}
		catch (final Exception ex) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
		}
	}

	/**
	 * Open the selected DTO bean in the editor
	 * @param item
	 */
	public void openDTOBeanInEditor(TreeItem item) {
		final var dto = (DTOBean) item.getData();

		try {
			EclipseIDEService.openInEditor(dto.getSourceFile());
		}
		catch (final Exception ex) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
		}
	}

	/**
	 * Open the selected form in the editor
	 * @param item
	 */
	public void openFormInEditor(TreeItem item) {
		final var form = (Form) item.getData();

		try {
			final WorkspaceFile typeScriptFile = form.getTypeScriptSourceFile();

			if (typeScriptFile != null)
				EclipseIDEService.openInEditor(typeScriptFile);
			else
				EclipseIDEService.openInEditor(form.getSourceFile());
		}
		catch (final Exception ex) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
		}
	}

	/**
	 * Open the selected grid panel in the editor
	 * @param item
	 */
	public void openGridPanelInEditor(TreeItem item) {
		final var panel = (FormPanel) item.getData();

		try {
			final WorkspaceFile typeScriptFile = panel.getTypeScriptSourceFile();

			if (typeScriptFile != null)
				EclipseIDEService.openInEditor(typeScriptFile);
			else
				EclipseIDEService.openInEditor(panel.getSourceFile());
		}
		catch (final Exception ex) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
		}
	}

	/**
	 * Open the selected repository in the editor
	 * @param item
	 */
	public void openRepositoryInEditor(TreeItem item) {
		final var repository = (Repository) item.getData();

		try {
			EclipseIDEService.openInEditor(repository.getSourceFile());
		}
		catch (final Exception ex) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
		}
	}

	/**
	 * Open the selected boundary bean in the editor
	 * @param item
	 */
	public void openBoundaryInEditor(TreeItem item) {
		final var boundaryBean = (BoundaryBean) item.getData();

		try {
			EclipseIDEService.openInEditor(boundaryBean.getBeanSourceFile());
		}
		catch (final Exception ex) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
		}
	}

	/**
	 * Open the selected exchange mapping object in the editor
	 * @param item
	 */
	public void openExchangeMappingObjectInEditor(TreeItem item) {
		final var exchangeMappingObject = (ExchangeMappingObject) item.getData();

		try {
			EclipseIDEService.openInEditor(exchangeMappingObject.getSourceFile());
		}
		catch (final Exception ex) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
		}
	}

	/**
	 * Open the selected data exchange service bean in the editor
	 * @param item
	 */
	public void openExchangeServiceBeanInEditor(TreeItem item) {
		final var exchangeBean = (DataExchangeServiceBean) item.getData();

		try {
			EclipseIDEService.openInEditor(exchangeBean.getSourceFile());
		}
		catch (final Exception ex) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
		}
	}

	/**
	 * Open the selected integration bean in the editor
	 * @param item
	 */
	public void openIntegrationBeanInEditor(TreeItem item) {
		final var integrationBean = (AbstractIntegrationBean) item.getData();

		try {
			EclipseIDEService.openInEditor(integrationBean.getServiceBeanSourceFile());
		}
		catch (final Exception ex) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
		}
	}

	/**
	 * Open the selected test case in the editor
	 * @param item
	 */
	public void openGUITestCaseInEditor(TreeItem item) {
		final var testCase = (GUITestCase) item.getData();

		try {
			EclipseIDEService.openInEditor(testCase.getSourceFile());
		}
		catch (final Exception ex) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
		}
	}

	/**
	 * Open the selected integration test case in the editor
	 * @param item
	 */
	public void openIntegrationTestCaseInEditor(TreeItem item) {
		final var testCase = (IntegrationTestCase) item.getData();

		try {
			EclipseIDEService.openInEditor(testCase.getSourceFile());
		}
		catch (final Exception ex) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
		}
	}

	/**
	 * Open the selected test suite in the editor
	 * @param item
	 */
	public void openTestSuiteInEditor(TreeItem item) {
		final var testSuite = (TestSuite) item.getData();

		try {
			EclipseIDEService.openInEditor(testSuite.getSourceFile());
		}
		catch (final Exception ex) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
		}
	}

	/**
	 * @return the tree component of the view
	 */
	public Tree getTree() {
		return tree;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
	 */
	@Override
	public void resourceChanged(IResourceChangeEvent event) {
		if (event.getType() != IResourceChangeEvent.POST_CHANGE)
			return;

		final IResourceDelta delta = event.getDelta();

		try {
			delta.accept(deltaToAccept -> {
				if (deltaToAccept.getKind() == IResourceDelta.ADDED && deltaToAccept.getResource().getType() == IResource.FILE) {
					final String extension = deltaToAccept.getResource().getFullPath().getFileExtension();

					// Refresh the project tree when a new diagram file has been created
					if (DIAGRAM_FILE_EXTENSION.equals(extension))
						shell.getDisplay().syncExec(this::refreshTree);
				}
				else if (deltaToAccept.getKind() == IResourceDelta.REMOVED && deltaToAccept.getResource().getType() == IResource.FOLDER) {
					final String folderName = deltaToAccept.getResource().getFullPath().lastSegment();

					// Refresh the project tree when the model folder has been deleted
					if (Constants.MODEL_FOLDER.equals(folderName))
						shell.getDisplay().syncExec(this::refreshTree);
				}

				// Avoid listening to unnecessary events
				if (deltaToAccept.getKind() == IResourceDelta.CHANGED || deltaToAccept.getKind() == IResourceDelta.REMOVED) {
					if (deltaToAccept.getResource().getType() == IResource.ROOT
							|| deltaToAccept.getResource().getType() == IResource.PROJECT)
						return true;

					if (deltaToAccept.getResource().getType() == IResource.FOLDER) {
						final String folderName = deltaToAccept.getResource().getFullPath().lastSegment();

						return Constants.MODEL_FOLDER.equals(folderName);
					}
				}

				return false;
			});
		}
		catch (final Exception _) {
			// Ignore all kinds of exceptions here!
		}
	}

}
