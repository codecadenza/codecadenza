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
package net.codecadenza.eclipse.ui.dialog.integration;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.IntegrationFactory;
import net.codecadenza.eclipse.model.integration.JMSIntegrationBean;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationBean;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod;
import net.codecadenza.eclipse.model.integration.MediaTypeEnumeration;
import net.codecadenza.eclipse.model.integration.RESTIntegrationBean;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;
import net.codecadenza.eclipse.model.integration.SOAPIntegrationBean;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.service.ServiceMethod;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.dto.DTOBeanService;
import net.codecadenza.eclipse.service.integration.IntegrationBeanService;
import net.codecadenza.eclipse.service.integration.IntegrationMethodInitService;
import net.codecadenza.eclipse.service.repository.RepositoryService;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaTitleAreaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.boundary.EditBoundaryMethodDialog;
import net.codecadenza.eclipse.ui.dialog.util.DomainObjectProposalTextField;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceAdapter;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

/**
 * <p>
 * Dialog for creating and maintaining integration beans
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditIntegrationBeanDialog extends CodeCadenzaTitleAreaDialog {
	private static final String DLG_TITLE_EDIT = "Edit integration bean";
	private static final String DLG_TITLE_NEW = "Create new integration bean";
	private static final String TOPIC_NAME_REGEX = "[\\w\\.-]{1,}";
	private static final Pattern TOPIC_NAME_PATTERN = Pattern.compile(TOPIC_NAME_REGEX);

	private final IntegrationModule module;
	private final Project project;
	private final IntegrationBeanService integrationBeanService;
	private final DTOBeanService dtoService;
	private final BoundaryService boundaryService;
	private AbstractIntegrationBean integrationBean;
	private boolean doEdit;
	private String title = DLG_TITLE_NEW;
	private Text txtInterfaceName;
	private Text txtDomainObject;
	private Text txtBeanName;
	private Text txtClientClassName;
	private Text txtProducerClassName;
	private Text txtComment;
	private Tree treeAvailableMethods;
	private Tree treeIntegrationMethods;
	private Button chkRPCStyle;
	private Button chkBareParamStyle;
	private Text txtServiceName;
	private Text txtPortName;
	private Text txtPortTypeName;
	private Text txtPath;
	private Text txtRequestTopic;
	private Text txtResponseTopic;
	private Text txtConsumerGroup;
	private Text txtRequestDestination;
	private Text txtResponseDestination;
	private Button chkRequestDestinationTopic;
	private Button chkResponseDestinationTopic;
	private Combo cboDefaultContentType;
	private Menu mnuIntegrationMethod;
	private BoundaryBean changedBoundaryBean;

	/**
	 * Constructor
	 * @param parentShell
	 * @param module
	 */
	public EditIntegrationBeanDialog(Shell parentShell, IntegrationModule module) {
		super(parentShell);

		this.module = module;
		this.project = module.getProject();
		this.boundaryService = new BoundaryService(project);
		this.integrationBeanService = new IntegrationBeanService(project);
		this.dtoService = new DTOBeanService(project);
	}

	/**
	 * Constructor
	 * @param parentShell
	 * @param integrationBean
	 */
	public EditIntegrationBeanDialog(Shell parentShell, AbstractIntegrationBean integrationBean) {
		super(parentShell);

		this.integrationBean = integrationBean;
		this.project = integrationBean.getNamespace().getProject();
		this.module = integrationBean.getIntegrationModule();
		this.doEdit = true;
		this.title = DLG_TITLE_EDIT;
		this.boundaryService = new BoundaryService(project);
		this.integrationBeanService = new IntegrationBeanService(project);
		this.dtoService = new DTOBeanService(project);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var tabFolderMain = new TabFolder(panDialogArea, SWT.NONE);
		tabFolderMain.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

		final var tabItemBasic = new TabItem(tabFolderMain, SWT.NONE);
		tabItemBasic.setText("Basic data");

		final var panBasic = new Composite(tabFolderMain, SWT.NONE);
		panBasic.setLayout(new GridLayout(4, false));

		tabItemBasic.setControl(panBasic);

		if (!doEdit) {
			final var lblDomainObject = new Label(panBasic, SWT.NONE);
			lblDomainObject.setText("Domain object:");

			final var propDomainObject = new DomainObjectProposalTextField(panBasic, project) {
				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.eclipse.ui.dialog.util.DomainObjectProposalTextField#getProposalData(java.lang.String)
				 */
				@Override
				public Collection<DomainObject> getProposalData(String filter) {
					final List<JavaType> types = module.getNamespace().getJavaTypes();
					final Set<DomainObject> domainSet = types.stream().map(AbstractIntegrationBean.class::cast)
							.map(AbstractIntegrationBean::getDomainObject).collect(Collectors.toSet());

					if (integrationBean != null) {
						// Remove the existing integration bean from the namespace in order to let it appear afterwards in the selection list!
						module.getNamespace().getJavaTypes().remove(integrationBean);
						integrationBean = null;
					}

					resetDialog();

					// Remove all domain objects that are referenced by existing integration beans of this module!
					final List<DomainObject> domainObjects = project.searchDomainObjectByName(filter, true, true, false);
					domainObjects.removeAll(domainSet);

					return domainObjects;
				}

				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#onProposalAccepted(java.lang.Object)
				 */
				@Override
				public void onProposalAccepted(DomainObject domainObject) {
					initDialog(domainObject);
				}
			};

			final var gdDomainObject = new GridData(SWT.FILL, SWT.CENTER, true, false);
			gdDomainObject.widthHint = 200;

			propDomainObject.setLayoutData(gdDomainObject);
		}
		else {
			final var lblDomainObject = new Label(panBasic, SWT.NONE);
			lblDomainObject.setText("Domain object:");

			final var gdDomainObject = new GridData(SWT.FILL, SWT.CENTER, true, false);
			gdDomainObject.widthHint = 200;

			txtDomainObject = new Text(panBasic, SWT.BORDER);
			txtDomainObject.setLayoutData(gdDomainObject);
			txtDomainObject.setEditable(false);
			txtDomainObject.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND));
		}

		final var lblBeanName = new Label(panBasic, SWT.NONE);
		lblBeanName.setText("Bean name:");

		final var gdBeanName = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdBeanName.widthHint = 200;

		txtBeanName = new Text(panBasic, SWT.BORDER);
		txtBeanName.setLayoutData(gdBeanName);

		if (module.hasSEIArtifact() && !(module.getTechnology() == IntegrationTechnology.KAFKA && !module.hasClientArtifact())) {
			final var lblInterfaceName = new Label(panBasic, SWT.NONE);
			lblInterfaceName.setText("Interface name:");

			txtInterfaceName = new Text(panBasic, SWT.BORDER);
			txtInterfaceName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		}

		if (module.hasClientArtifact()) {
			final var lblClientClassName = new Label(panBasic, SWT.NONE);
			lblClientClassName.setText("Client class name:");

			txtClientClassName = new Text(panBasic, SWT.BORDER);
			txtClientClassName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			if (module.isAddProducers()) {
				final var lblProducerClassName = new Label(panBasic, SWT.NONE);
				lblProducerClassName.setText("Producer class name:");

				txtProducerClassName = new Text(panBasic, SWT.BORDER);
				txtProducerClassName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			}
		}

		// Add fields that depend on the selected integration technology
		if (module.getTechnology() == IntegrationTechnology.SOAP) {
			final var lblRPCStyle = new Label(panBasic, SWT.NONE);
			lblRPCStyle.setText("RPC style:");

			chkRPCStyle = new Button(panBasic, SWT.CHECK);

			final var lblBareParamStyle = new Label(panBasic, SWT.NONE);
			lblBareParamStyle.setText("Bare parameter style:");

			chkBareParamStyle = new Button(panBasic, SWT.CHECK);

			final var lblServiceName = new Label(panBasic, SWT.NONE);
			lblServiceName.setText("Service name:");

			txtServiceName = new Text(panBasic, SWT.BORDER);
			txtServiceName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			final var lblPortName = new Label(panBasic, SWT.NONE);
			lblPortName.setText("Port name:");

			txtPortName = new Text(panBasic, SWT.BORDER);
			txtPortName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			final var lblPortTypeName = new Label(panBasic, SWT.NONE);
			lblPortTypeName.setText("Port type name:");

			txtPortTypeName = new Text(panBasic, SWT.BORDER);
			txtPortTypeName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		}
		else if (module.getTechnology() == IntegrationTechnology.REST) {
			final var lblPath = new Label(panBasic, SWT.NONE);
			lblPath.setText("Path:");

			txtPath = new Text(panBasic, SWT.BORDER);
			txtPath.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			final var lblDefaultContentType = new Label(panBasic, SWT.NONE);
			lblDefaultContentType.setText("Default content type:");

			cboDefaultContentType = new Combo(panBasic, SWT.READ_ONLY);
			cboDefaultContentType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		}
		else if (module.getTechnology() == IntegrationTechnology.KAFKA) {
			final var lblRequestTopic = new Label(panBasic, SWT.NONE);
			lblRequestTopic.setText("Request topic:");

			txtRequestTopic = new Text(panBasic, SWT.BORDER);
			txtRequestTopic.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			final var lblResponseTopic = new Label(panBasic, SWT.NONE);
			lblResponseTopic.setText("Response topic:");

			txtResponseTopic = new Text(panBasic, SWT.BORDER);
			txtResponseTopic.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			final var lblConsumerGroup = new Label(panBasic, SWT.NONE);
			lblConsumerGroup.setText("Consumer group:");

			txtConsumerGroup = new Text(panBasic, SWT.BORDER);
			txtConsumerGroup.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		}
		else if (module.getTechnology() == IntegrationTechnology.JMS) {
			if (module.isAddProducers()) {
				new Label(panBasic, SWT.NONE);
				new Label(panBasic, SWT.NONE);
			}

			final var lblRequestDestination = new Label(panBasic, SWT.NONE);
			lblRequestDestination.setText("Request destination:");

			txtRequestDestination = new Text(panBasic, SWT.BORDER);
			txtRequestDestination.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			chkRequestDestinationTopic = new Button(panBasic, SWT.CHECK);
			chkRequestDestinationTopic.setText("is JMS topic");

			new Label(panBasic, SWT.NONE);

			final var lblResponseDestination = new Label(panBasic, SWT.NONE);
			lblResponseDestination.setText("Response destination:");

			txtResponseDestination = new Text(panBasic, SWT.BORDER);
			txtResponseDestination.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			chkResponseDestinationTopic = new Button(panBasic, SWT.CHECK);
			chkResponseDestinationTopic.setText("is JMS topic");
		}

		final var tabItemComment = new TabItem(tabFolderMain, SWT.NONE);
		tabItemComment.setText("Comment");

		final var panComment = new Composite(tabFolderMain, SWT.NONE);
		panComment.setLayout(new GridLayout(2, false));

		tabItemComment.setControl(panComment);

		final var lblComment = new Label(panComment, SWT.NONE);
		lblComment.setText("Bean comment:");

		final var gdComment = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdComment.heightHint = 80;

		txtComment = new Text(panComment, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.H_SCROLL);
		txtComment.setLayoutData(gdComment);

		final var sashForm = new SashForm(panDialogArea, SWT.NONE);
		sashForm.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var panAvailableMethods = new Composite(sashForm, SWT.NONE);
		panAvailableMethods.setLayout(new GridLayout());

		final var lblAvailableMethods = new Label(panAvailableMethods, SWT.NONE);
		lblAvailableMethods.setFont(JFaceResources.getBannerFont());
		lblAvailableMethods.setText("Available methods");

		final var gdAvailableMethods = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdAvailableMethods.widthHint = 500;
		gdAvailableMethods.heightHint = 400;

		treeAvailableMethods = new Tree(panAvailableMethods, SWT.BORDER);
		treeAvailableMethods.setLayoutData(gdAvailableMethods);

		treeAvailableMethods.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseAdapter#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				onServiceMethodSelected();
			}
		});

		// Allow data to be copied or moved from the drag source
		final var source = new DragSource(treeAvailableMethods, DND.DROP_MOVE | DND.DROP_COPY);
		source.setTransfer(TextTransfer.getInstance());

		source.addDragListener(new DragSourceAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DragSourceAdapter#dragStart(org.eclipse.swt.dnd.DragSourceEvent)
			 */
			@Override
			public void dragStart(DragSourceEvent event) {
				if (getSelectedServiceMethod() != null)
					event.doit = true;
				else
					event.doit = false;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DragSourceAdapter#dragSetData(org.eclipse.swt.dnd.DragSourceEvent)
			 */
			@Override
			public void dragSetData(DragSourceEvent event) {
				final ServiceMethod serviceMethod = getSelectedServiceMethod();

				if (serviceMethod == null)
					return;

				event.data = serviceMethod.toString();
			}
		});

		final var panSelectedMethods = new Composite(sashForm, SWT.NONE);
		panSelectedMethods.setLayout(new GridLayout());

		sashForm.setWeights(1, 1);

		final var lblIntegrationMethods = new Label(panSelectedMethods, SWT.NONE);
		lblIntegrationMethods.setFont(JFaceResources.getBannerFont());
		lblIntegrationMethods.setText("Integration methods");

		final var gdIntegrationMethods = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdIntegrationMethods.widthHint = 500;
		gdIntegrationMethods.heightHint = 400;

		treeIntegrationMethods = new Tree(panSelectedMethods, SWT.BORDER);
		treeIntegrationMethods.setLayoutData(gdIntegrationMethods);

		treeIntegrationMethods.addMenuDetectListener(e -> {
			treeIntegrationMethods.setMenu(null);

			if (getSelectedIntegrationMethod() != null)
				treeIntegrationMethods.setMenu(mnuIntegrationMethod);
		});

		treeIntegrationMethods.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseAdapter#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				editSelectedIntegrationMethod();
			}
		});

		mnuIntegrationMethod = new Menu(treeIntegrationMethods);

		final var mniEdit = new MenuItem(mnuIntegrationMethod, SWT.NONE);
		mniEdit.setText("Edit");

		mniEdit.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				editSelectedIntegrationMethod();
			}
		});

		final var mniRemoveMethod = new MenuItem(mnuIntegrationMethod, SWT.NONE);
		mniRemoveMethod.setText("Remove");

		mniRemoveMethod.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final AbstractIntegrationMethod integrationMethod = getSelectedIntegrationMethod();

				if (integrationMethod == null)
					return;

				final Set<IntegrationTestCase> testCases = project.searchIntegrationTestCasesByIntegrationMethod(integrationMethod);

				if (!testCases.isEmpty()) {
					final var message = "The method cannot be removed, because it is referenced by following integration test cases:\n";

					final String testCaseNames = testCases.stream().map(IntegrationTestCase::getName)
							.collect(Collectors.joining(System.lineSeparator()));

					MessageDialog.openInformation(getShell(), "Remove", message + testCaseNames);
					return;
				}

				integrationBean.getMethods().remove(integrationMethod);

				// Refresh the tree views
				refreshIntegrationMethodTreeView();
				refreshServiceMethodTreeView(integrationBean.getDomainObject());
			}
		});

		// Initialize the drop target
		final var target = new DropTarget(treeIntegrationMethods, DND.DROP_COPY);

		// Receive the data in text format
		target.setTransfer(TextTransfer.getInstance());

		target.addDropListener(new DropTargetAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#dragEnter(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dragEnter(DropTargetEvent event) {
				event.detail = DND.DROP_COPY;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#drop(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void drop(DropTargetEvent event) {
				onServiceMethodSelected();
			}
		});

		if (doEdit) {
			initDialog();

			setTitle(DLG_TITLE_EDIT);
			setMessage("Change bean data");
		}
		else {
			setTitle(DLG_TITLE_NEW);
			setMessage("Select a domain object...");
		}

		return panDialogArea;
	}

	/**
	 * @return the selected service method in the tree view or null if no respective item is selected
	 */
	private ServiceMethod getSelectedServiceMethod() {
		TreeItem selItem = null;

		// Get the selected item
		final TreeItem[] selItems = treeAvailableMethods.getSelection();

		for (final TreeItem item : selItems)
			selItem = item;

		if (selItem == null || selItem.getData() == null || !(selItem.getData() instanceof final ServiceMethod serviceMethod))
			return null;

		return serviceMethod;
	}

	/**
	 * @return the selected integration method in the tree view or null if no respective item is selected
	 */
	private AbstractIntegrationMethod getSelectedIntegrationMethod() {
		TreeItem selItem = null;

		// Get the selected item
		final TreeItem[] selItems = treeIntegrationMethods.getSelection();

		for (final TreeItem item : selItems)
			selItem = item;

		if (selItem == null || selItem.getData() == null
				|| !(selItem.getData() instanceof final AbstractIntegrationMethod integrationMethod))
			return null;

		return integrationMethod;
	}

	/**
	 * Handle the selection of a service method in the tree view
	 */
	private void onServiceMethodSelected() {
		try {
			final ServiceMethod serviceMethod = getSelectedServiceMethod();

			if (serviceMethod == null)
				return;

			addIntegrationMethod(serviceMethod);
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
		}
	}

	/**
	 * Edit the selected integration method
	 */
	private void editSelectedIntegrationMethod() {
		final AbstractIntegrationMethod selectedMethod = getSelectedIntegrationMethod();

		if (selectedMethod == null)
			return;

		final var dlg = new EditIntegrationMethodDialog(getShell(), selectedMethod);

		if (dlg.open() == Dialog.OK)
			refreshIntegrationMethodTreeView();
	}

	/**
	 * Add the integration method to the respective bean
	 * @param serviceMethod
	 * @throws Exception if the initialization of a new integration method has failed
	 */
	private void addIntegrationMethod(ServiceMethod serviceMethod) throws Exception {
		final BoundaryMethod boundaryMethod;
		MediaTypeEnumeration mediaType = null;

		// If the service method represents a repository method we must initialize a corresponding boundary method first!
		if (serviceMethod instanceof final RepositoryMethod repositoryMethod) {
			final Repository repository = repositoryMethod.getRepository();

			changedBoundaryBean = boundaryService.getOrCreateBoundaryOfDomainObject(repositoryMethod.getRepository().getDomainObject());
			boundaryMethod = boundaryService.initializeBoundaryMethod(repository, repositoryMethod);

			final var dlg = new EditBoundaryMethodDialog(getShell(), changedBoundaryBean, boundaryMethod);

			if (dlg.open() != Dialog.OK)
				return;

			boundaryMethod.setBoundaryBean(changedBoundaryBean);
			changedBoundaryBean.getBoundaryMethods().add(boundaryMethod);
		}
		else
			boundaryMethod = (BoundaryMethod) serviceMethod;

		if (cboDefaultContentType != null)
			mediaType = MediaTypeEnumeration.valueOf(cboDefaultContentType.getItem(cboDefaultContentType.getSelectionIndex()));

		final AbstractIntegrationMethod integrationMethod = new IntegrationMethodInitService().initializeMethod(integrationBean,
				boundaryMethod, mediaType);

		integrationBean.getMethods().add(integrationMethod);

		// Refresh the tree views
		refreshIntegrationMethodTreeView();
		refreshServiceMethodTreeView(integrationBean.getDomainObject());
	}

	/**
	 * Initialize the dialog
	 */
	private void initDialog() {
		if (module.getTechnology() == IntegrationTechnology.SOAP) {
			final var soapBean = (SOAPIntegrationBean) integrationBean;

			txtPortName.setText(soapBean.getPortName());
			txtPortTypeName.setText(soapBean.getPortTypeName());
			txtServiceName.setText(soapBean.getServiceName());
			chkBareParamStyle.setSelection(soapBean.isBareParameterStyle());
			chkRPCStyle.setSelection(soapBean.isRpcStype());
		}
		else if (module.getTechnology() == IntegrationTechnology.REST) {
			final var restBean = (RESTIntegrationBean) integrationBean;
			MediaTypeEnumeration defaultMediaType = MediaTypeEnumeration.JSON;

			txtPath.setText(restBean.getPath());

			// Search for the default content type by checking existing methods
			for (final AbstractIntegrationMethod existingMethod : integrationBean.getMethods()) {
				final var restMethod = (RESTIntegrationMethod) existingMethod;

				if (restMethod.getOutputType() == MediaTypeEnumeration.XML || restMethod.getInputType() == MediaTypeEnumeration.XML) {
					defaultMediaType = MediaTypeEnumeration.XML;
					break;
				}
			}

			cboDefaultContentType.add(defaultMediaType.getName());

			if (defaultMediaType == MediaTypeEnumeration.XML)
				cboDefaultContentType.add(MediaTypeEnumeration.JSON.getName());
			else
				cboDefaultContentType.add(MediaTypeEnumeration.XML.getName());

			cboDefaultContentType.select(0);
		}
		else if (module.getTechnology() == IntegrationTechnology.KAFKA) {
			final var kafkaBean = (KafkaIntegrationBean) integrationBean;

			txtRequestTopic.setText(kafkaBean.getRequestTopic());
			txtResponseTopic.setText(kafkaBean.getResponseTopic());
			txtConsumerGroup.setText(kafkaBean.getConsumerGroup());
		}
		else if (module.getTechnology() == IntegrationTechnology.JMS) {
			final var jmsBean = (JMSIntegrationBean) integrationBean;

			txtRequestDestination.setText(jmsBean.getRequestDestination().getName());
			txtResponseDestination.setText(jmsBean.getResponseDestination().getName());
			chkRequestDestinationTopic.setSelection(jmsBean.getRequestDestination().isTopic());
			chkResponseDestinationTopic.setSelection(jmsBean.getResponseDestination().isTopic());
		}

		txtDomainObject.setText(integrationBean.getDomainObject().getName());
		txtBeanName.setText(integrationBean.getName());
		txtComment.setText(integrationBean.getComment());

		if (txtInterfaceName != null)
			txtInterfaceName.setText(integrationBean.getInterfaceName());

		if (txtClientClassName != null)
			txtClientClassName.setText(integrationBean.getClientClassName());

		if (txtProducerClassName != null)
			txtProducerClassName.setText(integrationBean.getProducerClassName());

		// Refresh the tree views
		refreshIntegrationMethodTreeView();
		refreshServiceMethodTreeView(integrationBean.getDomainObject());
	}

	/**
	 * Reset the dialog
	 */
	private void resetDialog() {
		if (module.getTechnology() == IntegrationTechnology.SOAP) {
			txtPortName.setText("");
			txtPortTypeName.setText("");
			txtServiceName.setText("");
			chkBareParamStyle.setSelection(false);
			chkRPCStyle.setSelection(false);
		}
		else if (module.getTechnology() == IntegrationTechnology.REST)
			txtPath.setText("");
		else if (module.getTechnology() == IntegrationTechnology.KAFKA) {
			txtRequestTopic.setText("");
			txtResponseTopic.setText("");
			txtConsumerGroup.setText("");
		}
		else if (module.getTechnology() == IntegrationTechnology.JMS) {
			txtRequestDestination.setText("");
			txtResponseDestination.setText("");
			chkRequestDestinationTopic.setSelection(false);
			chkResponseDestinationTopic.setSelection(false);
		}

		txtBeanName.setText("");
		txtComment.setText("");

		if (txtInterfaceName != null)
			txtInterfaceName.setText("");

		if (txtClientClassName != null)
			txtClientClassName.setText("");

		if (txtProducerClassName != null)
			txtProducerClassName.setText("");

		// Refresh the tree views
		refreshIntegrationMethodTreeView();
		refreshServiceMethodTreeView(null);
	}

	/**
	 * Initialize the dialog
	 * @param domainObject
	 */
	private void initDialog(DomainObject domainObject) {
		final var interfaceName = domainObject.getName() + module.getTechnology().getName() + "Service";
		final var clientClassName = domainObject.getName() + module.getTechnology().getName() + "Client";
		final var producerClassName = interfaceName + "Producer";
		final String beanName;
		final String comment;

		if (module.getTechnology() == IntegrationTechnology.KAFKA) {
			beanName = domainObject.getName() + module.getTechnology().getName() + "Consumer";
			comment = "Consumer for Kafka messages related to " + domainObject.getLabel() + " objects";
		}
		else if (module.getTechnology() == IntegrationTechnology.JMS) {
			beanName = domainObject.getName() + module.getTechnology().getName() + "Consumer";
			comment = "Consumer for JMS messages related to " + domainObject.getLabel() + " objects";
		}
		else {
			beanName = domainObject.getName() + module.getTechnology().getName() + "ServiceBean";
			comment = module.getTechnology().getName() + " service for " + domainObject.getLabel() + " objects";
		}

		if (module.getTechnology() == IntegrationTechnology.SOAP) {
			final SOAPIntegrationBean soapBean = IntegrationFactory.eINSTANCE.createSOAPIntegrationBean();

			txtPortName.setText(domainObject.getName() + "ServicePort");
			txtPortTypeName.setText(domainObject.getName() + "ServicePortType");
			txtServiceName.setText(domainObject.getName() + "Service");

			integrationBean = soapBean;
		}
		else if (module.getTechnology() == IntegrationTechnology.REST) {
			final RESTIntegrationBean restBean = IntegrationFactory.eINSTANCE.createRESTIntegrationBean();

			txtPath.setText(domainObject.getName().toLowerCase());

			cboDefaultContentType.add(MediaTypeEnumeration.JSON.getName());
			cboDefaultContentType.add(MediaTypeEnumeration.XML.getName());
			cboDefaultContentType.select(0);

			integrationBean = restBean;
		}
		else if (module.getTechnology() == IntegrationTechnology.RMI)
			integrationBean = IntegrationFactory.eINSTANCE.createRMIIntegrationBean();
		else if (module.getTechnology() == IntegrationTechnology.KAFKA) {
			final KafkaIntegrationBean kafkaBean = IntegrationFactory.eINSTANCE.createKafkaIntegrationBean();

			txtRequestTopic.setText(domainObject.getName().toLowerCase() + "-request");
			txtResponseTopic.setText(domainObject.getName().toLowerCase() + "-response");
			txtConsumerGroup.setText(domainObject.getName().toLowerCase() + "-group");

			integrationBean = kafkaBean;
		}
		else if (module.getTechnology() == IntegrationTechnology.JMS) {
			final JMSIntegrationBean jmsBean = IntegrationFactory.eINSTANCE.createJMSIntegrationBean();
			jmsBean.setRequestDestination(IntegrationFactory.eINSTANCE.createJMSResource());
			jmsBean.setResponseDestination(IntegrationFactory.eINSTANCE.createJMSResource());

			txtRequestDestination.setText(getDefaultJMSDestinationName(domainObject, true));
			txtResponseDestination.setText(getDefaultJMSDestinationName(domainObject, false));

			integrationBean = jmsBean;
		}

		integrationBean.setName(beanName);
		integrationBean.setDomainObject(domainObject);
		integrationBean.setNamespace(module.getNamespace());

		txtBeanName.setText(beanName);
		txtComment.setText(comment);

		if (txtInterfaceName != null)
			txtInterfaceName.setText(interfaceName);

		if (txtClientClassName != null)
			txtClientClassName.setText(clientClassName);

		if (txtProducerClassName != null)
			txtProducerClassName.setText(producerClassName);

		// Initialize the tree views
		refreshIntegrationMethodTreeView();
		refreshServiceMethodTreeView(domainObject);
	}

	/**
	 * Refresh the tree view that contains all available service methods
	 * @param domainObject
	 */
	private void refreshServiceMethodTreeView(DomainObject domainObject) {
		// Remove all existing tree items
		treeAvailableMethods.removeAll();

		if (integrationBean == null)
			return;

		final Optional<Repository> optionalRepo = project.getAllRepositoriesOfProject().stream()
				.filter(e -> e.getDomainObject().equals(domainObject)).findFirst();
		final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(domainObject);

		if (boundaryBean == null || !optionalRepo.isPresent())
			return;

		final Repository repository = optionalRepo.get();

		final var boundaryBeanItem = new TreeItem(treeAvailableMethods, SWT.NONE);
		boundaryBeanItem.setText(boundaryBean.getName());
		boundaryBeanItem.setData(boundaryBean);
		boundaryBeanItem.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_CLASS));

		for (final BoundaryMethod boundaryMethod : boundaryBean.getBoundaryMethods()) {
			// Don't display a boundary method that is already referenced by an integration method!
			if (integrationBean.getMethods().stream().anyMatch(e -> e.getBoundaryMethod().equals(boundaryMethod)))
				continue;

			// A REST method with more than one content parameter is not allowed!
			if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.REST
					&& boundaryMethod.getMethodType() == BoundaryMethodTypeEnumeration.CREATE
					&& boundaryMethod.getMethodParameters().size() > 1)
				continue;

			final var methodItem = new TreeItem(boundaryBeanItem, SWT.NONE);
			methodItem.setData(boundaryMethod);
			methodItem.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PUBLIC));
			methodItem.setText(boundaryService.getBoundaryMethodSignature(boundaryMethod));
		}

		// Add repository methods
		if (project.isBoundaryMode()) {
			final var repositoryItem = new TreeItem(treeAvailableMethods, SWT.NONE);
			repositoryItem.setText(repository.getName());
			repositoryItem.setData(repository);
			repositoryItem.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_CLASS));

			addRepositoryMethods(repository, repositoryItem);

			repositoryItem.setExpanded(true);
		}
		else
			addRepositoryMethods(repository, boundaryBeanItem);

		boundaryBeanItem.setExpanded(true);
	}

	/**
	 * Add repository methods to the given parent tree item
	 * @param repository
	 * @param parentItem
	 */
	private void addRepositoryMethods(Repository repository, TreeItem parentItem) {
		final var repositoryService = new RepositoryService(project);

		for (final RepositoryMethod repositoryMethod : repository.getRepositoryMethods()) {
			final RepositoryMethodTypeEnumeration repositoryMethodType = repositoryMethod.getMethodType();

			if (!project.isBoundaryMode() && repositoryMethod.getMethodType() == RepositoryMethodTypeEnumeration.GET_ASSOCIATION)
				continue;

			// Repository methods with inappropriate types must not be displayed either!
			if (Stream.of(BoundaryMethodTypeEnumeration.values()).map(BoundaryMethodTypeEnumeration::name)
					.noneMatch(e -> e.equals(repositoryMethodType.name())))
				continue;

			final var methodItem = new TreeItem(parentItem, SWT.NONE);
			methodItem.setData(repositoryMethod);
			methodItem.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PUBLIC));
			methodItem.setText(repositoryService.getMethodSignature(repositoryMethod));
		}
	}

	/**
	 * Refresh the tree view that contains all integration methods
	 */
	private void refreshIntegrationMethodTreeView() {
		treeIntegrationMethods.removeAll();

		if (integrationBean == null)
			return;

		final var integrationBeanItem = new TreeItem(treeIntegrationMethods, SWT.NONE);
		integrationBeanItem.setText(integrationBean.getName());
		integrationBeanItem.setData(integrationBean);
		integrationBeanItem.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_CLASS));

		integrationBean.getMethods().forEach(integrationMethod -> {
			final var methodItem = new TreeItem(integrationBeanItem, SWT.NONE);
			methodItem.setData(integrationMethod);
			methodItem.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PUBLIC));
			methodItem.setText(integrationBeanService.getMethodSignature(integrationMethod));
		});

		integrationBeanItem.setExpanded(true);
	}

	/**
	 * @return true if the validation was successful
	 */
	private boolean checkInput() {
		if (integrationBean == null) {
			setErrorMessage("A domain object must be selected!");
			return false;
		}

		if (!validateJavaTypeName(txtBeanName))
			return false;

		if (isInterfaceMandatory()) {
			if (txtInterfaceName != null && !validateJavaTypeName(txtInterfaceName))
				return false;
		}
		else if (txtInterfaceName != null && !txtInterfaceName.getText().isEmpty() && !validateJavaTypeName(txtInterfaceName))
			return false;

		if (txtClientClassName != null && !txtClientClassName.getText().isEmpty() && !validateJavaTypeName(txtClientClassName))
			return false;

		if (txtProducerClassName != null && !txtProducerClassName.getText().isEmpty() && !validateJavaTypeName(txtProducerClassName))
			return false;

		if (txtClientClassName != null && txtProducerClassName != null && txtClientClassName.getText().isEmpty()
				&& !txtProducerClassName.getText().isEmpty()) {
			setErrorMessage("An integration service producer class without a respective client is not supported!");
			txtProducerClassName.setFocus();
			return false;
		}

		if (txtRequestTopic != null && !validateTopicName(txtRequestTopic))
			return false;

		if (txtResponseTopic != null && !validateTopicName(txtResponseTopic))
			return false;

		if (txtConsumerGroup != null && txtConsumerGroup.getText().isEmpty()) {
			setErrorMessage("A consumer group ID must be entered!");
			txtConsumerGroup.setFocus();
			return false;
		}

		final String beanName = txtBeanName.getText();
		final String interfaceName = txtInterfaceName != null ? txtInterfaceName.getText() : null;
		final String clientName = txtClientClassName != null ? txtClientClassName.getText() : null;
		final String producerName = txtProducerClassName != null ? txtProducerClassName.getText() : null;

		// Check if the class names are unique
		final String uniqueCheckMessage = integrationBeanService.checkUniqueClassNames(module, integrationBean, beanName,
				interfaceName, clientName, producerName);

		if (uniqueCheckMessage != null) {
			setErrorMessage(uniqueCheckMessage);
			return false;
		}

		if (txtServiceName != null && !validateJavaTypeName(txtServiceName))
			return false;

		if (txtPortName != null && !validateJavaTypeName(txtPortName))
			return false;

		if (txtPortTypeName != null && !validateJavaTypeName(txtPortTypeName))
			return false;

		// Search for duplicate method signatures
		final var signatureSet = new HashSet<String>();

		for (final AbstractIntegrationMethod integrationMethod : integrationBean.getMethods()) {
			final String signature = integrationBeanService.getMethodSignature(integrationMethod);

			if (signatureSet.contains(signature)) {
				setErrorMessage("A method with the signature '" + signature + "' already exists!");
				return false;
			}

			signatureSet.add(signature);
		}

		if (module.getTechnology() == IntegrationTechnology.SOAP) {
			final var nameSet = new HashSet<String>();

			// Search for SOAP methods that have the same name!
			for (final AbstractIntegrationMethod integrationMethod : integrationBean.getMethods()) {
				if (nameSet.contains(integrationMethod.getName())) {
					setErrorMessage("SOAP methods with the same name '" + integrationMethod.getName() + "' are not supported!");
					return false;
				}

				nameSet.add(integrationMethod.getName());
			}
		}
		else if (module.getTechnology() == IntegrationTechnology.KAFKA) {
			// Search for duplicate schema names
			for (final AbstractIntegrationMethod method : integrationBean.getMethods()) {
				final var kafkaMethod = (KafkaIntegrationMethod) method;
				String schemaName = kafkaMethod.getRequestSchemaName();
				boolean valid = integrationBeanService.validateSchemaName(method, schemaName);

				if (valid) {
					schemaName = kafkaMethod.getResponseSchemaName();
					valid = integrationBeanService.validateSchemaName(method, schemaName);
				}

				if (!valid) {
					setErrorMessage("A Kafka method with the schema name '" + schemaName + "' already exists!");
					return false;
				}
			}
		}
		else if (module.getTechnology() == IntegrationTechnology.JMS) {
			final JMSIntegrationBean jmsBean = (JMSIntegrationBean) integrationBean;
			final String requestDestinationName = txtRequestDestination.getText();
			final String responseDestinationName = txtResponseDestination.getText();

			final String validationErrorRequest = validateJMSDestination(jmsBean, requestDestinationName);

			if (validationErrorRequest != null) {
				setErrorMessage(validationErrorRequest);
				txtRequestTopic.setFocus();
				return false;
			}

			final String validationErrorResponse = validateJMSDestination(jmsBean, responseDestinationName);

			if (validationErrorResponse != null) {
				setErrorMessage(validationErrorResponse);
				txtResponseTopic.setFocus();
				return false;
			}

			if (requestDestinationName.equals(responseDestinationName)) {
				setErrorMessage("The JMS destination names must be different!");
				return false;
			}
		}

		return true;
	}

	/**
	 * Check if the name of a JMS destination is valid
	 * @param jmsBean
	 * @param destinationName
	 * @return an error message if the validation has failed
	 */
	private String validateJMSDestination(JMSIntegrationBean jmsBean, String destinationName) {
		if (destinationName.isEmpty())
			return "The name of a JMS destination must not be empty!";

		final boolean valid = integrationBeanService.validateDestinationName(jmsBean, destinationName);

		if (!valid)
			return "Another JMS destination with the name '" + destinationName + "' already exists!";

		return null;
	}

	/**
	 * @param textField
	 * @return true if the text in the given field represents a valid Java type name
	 */
	private boolean validateJavaTypeName(Text textField) {
		final IStatus status = EclipseIDEService.validateJavaTypeName(textField.getText());

		if (status.getSeverity() > IStatus.INFO) {
			setErrorMessage(status.getMessage());
			textField.setFocus();
			return false;
		}

		return true;
	}

	/**
	 * @param textField
	 * @return true if the text in the given field represents a valid Kafka topic name
	 */
	private boolean validateTopicName(Text textField) {
		if (!TOPIC_NAME_PATTERN.matcher(textField.getText()).matches()) {
			setErrorMessage("The topic name must match the regular expression " + TOPIC_NAME_REGEX + "!");
			textField.setFocus();
			return false;
		}

		return true;
	}

	/**
	 * @return true if a service interface is mandatory
	 */
	private boolean isInterfaceMandatory() {
		if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.SOAP
				|| integrationBean.getIntegrationTechnology() == IntegrationTechnology.RMI)
			return true;

		// In case of either a REST or a Kafka service an interface must be created if a client should be generated!
		return txtClientClassName != null && !txtClientClassName.getText().isEmpty();
	}

	/**
	 * Save the integration bean
	 * @throws Exception if the save operation has failed
	 */
	private void saveIntegrationBean() throws Exception {
		if (doEdit) {
			final String existingBeanName = integrationBean.getName();
			final String existingInterfaceName = integrationBean.getInterfaceName();
			final String existingClientName = integrationBean.getClientClassName();
			final String existingProducerName = integrationBean.getProducerClassName();

			// Rename the existing service bean
			if (!existingBeanName.equals(txtBeanName.getText()))
				EclipseIDEService.renameCompUnit(integrationBean.getServiceBeanSourceFile(), txtBeanName.getText());

			if (txtInterfaceName != null && existingInterfaceName != null && !existingInterfaceName.isEmpty()
					&& !txtInterfaceName.getText().equals(existingInterfaceName)) {
				// Delete or rename the existing interface
				if (txtInterfaceName.getText().isEmpty())
					EclipseIDEService.deleteSource(integrationBean.getSEISourceFile());
				else
					EclipseIDEService.renameCompUnit(integrationBean.getSEISourceFile(), txtInterfaceName.getText());
			}

			if (txtClientClassName != null && existingClientName != null && !existingClientName.isEmpty()
					&& !txtClientClassName.getText().equals(existingClientName)) {
				// Delete or rename the existing client
				if (txtClientClassName.getText().isEmpty())
					EclipseIDEService.deleteSource(integrationBean.getClientSourceFile());
				else
					EclipseIDEService.renameCompUnit(integrationBean.getClientSourceFile(), txtClientClassName.getText());
			}

			if (txtProducerClassName != null && existingProducerName != null && !existingProducerName.isEmpty()
					&& !txtProducerClassName.getText().equals(existingProducerName)) {
				// Delete or rename the existing producer
				if (txtProducerClassName.getText().isEmpty())
					EclipseIDEService.deleteSource(integrationBean.getProducerSourceFile());
				else
					EclipseIDEService.renameCompUnit(integrationBean.getProducerSourceFile(), txtProducerClassName.getText());
			}
		}

		integrationBean.setName(txtBeanName.getText());
		integrationBean.setComment(txtComment.getText());

		if (txtInterfaceName != null)
			integrationBean.setInterfaceName(txtInterfaceName.getText());

		if (txtClientClassName != null)
			integrationBean.setClientClassName(txtClientClassName.getText());

		if (txtProducerClassName != null)
			integrationBean.setProducerClassName(txtProducerClassName.getText());

		if (module.getTechnology() == IntegrationTechnology.SOAP) {
			final var soapBean = (SOAPIntegrationBean) integrationBean;
			soapBean.setPortName(txtPortName.getText());
			soapBean.setPortTypeName(txtPortTypeName.getText());
			soapBean.setServiceName(txtServiceName.getText());
			soapBean.setBareParameterStyle(chkBareParamStyle.getSelection());
			soapBean.setRpcStype(chkRPCStyle.getSelection());
			soapBean.setNamespace(module.getNamespace());
		}
		else if (module.getTechnology() == IntegrationTechnology.REST) {
			final var restBean = (RESTIntegrationBean) integrationBean;
			restBean.setPath(txtPath.getText());
		}
		else if (module.getTechnology() == IntegrationTechnology.KAFKA) {
			final var kafkaBean = (KafkaIntegrationBean) integrationBean;
			kafkaBean.setRequestTopic(txtRequestTopic.getText());
			kafkaBean.setResponseTopic(txtResponseTopic.getText());
			kafkaBean.setConsumerGroup(txtConsumerGroup.getText());
		}
		else if (module.getTechnology() == IntegrationTechnology.JMS) {
			final var jmsBean = (JMSIntegrationBean) integrationBean;
			jmsBean.getRequestDestination().setName(txtRequestDestination.getText());
			jmsBean.getResponseDestination().setName(txtResponseDestination.getText());
			jmsBean.getRequestDestination().setTopic(chkRequestDestinationTopic.getSelection());
			jmsBean.getResponseDestination().setTopic(chkResponseDestinationTopic.getSelection());
		}

		final Resource eResource = module.getNamespace().eResource();

		if (!eResource.getContents().contains(integrationBean))
			eResource.getContents().add(integrationBean);

		if (changedBoundaryBean != null && !eResource.getContents().contains(changedBoundaryBean))
			eResource.getContents().add(changedBoundaryBean);

		if (doEdit) {
			// Search for unused virtual facade methods and remove them
			boundaryService.removeUnusedVirtualMethods();

			// Search for unused virtual data transfer objects and delete them
			dtoService.removeUnusedVirtualDTOs();
		}

		EclipseIDEService.saveProjectMetaData(project);

		integrationBeanService.rebuildIntegrationBeanSourceFiles(integrationBean);

		if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.KAFKA)
			integrationBeanService.generateJavaClassesFromAvroIDLFiles();

		if (changedBoundaryBean != null)
			boundaryService.rebuildBoundarySourceFiles(changedBoundaryBean);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText(title);
	}

	/**
	 * Create the default name for a JMS destination
	 * @param domainObject
	 * @param request
	 * @return the default destination name
	 */
	private String getDefaultJMSDestinationName(DomainObject domainObject, boolean request) {
		final var name = new StringBuilder();

		if (!request && project.isDeployedOnJBoss())
			name.append("java:/");

		name.append("jms/");
		name.append(domainObject.getName());

		if (request)
			name.append("Request");
		else
			name.append("Response");

		name.append("Queue");

		return name.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			if (!checkInput())
				return;

			try {
				saveIntegrationBean();
			}
			catch (final Exception e) {
				CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
				return;
			}
		}

		super.buttonPressed(buttonId);
	}

}
