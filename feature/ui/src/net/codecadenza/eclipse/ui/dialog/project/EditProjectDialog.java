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
package net.codecadenza.eclipse.ui.dialog.project;

import static net.codecadenza.eclipse.shared.Constants.MODEL_FILE_EXTENSION;
import static net.codecadenza.eclipse.shared.Constants.MODEL_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.PACK_SERVICE;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_BEAN;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_LISTENER;

import net.codecadenza.eclipse.model.domain.DomainFactory;
import net.codecadenza.eclipse.model.domain.DomainNamespace;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.java.JavaFactory;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.MappingAnnotationStrategy;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.project.XMLMappingType;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.service.build.ProjectBuildFactory;
import net.codecadenza.eclipse.service.exchange.DataExchangeBeanService;
import net.codecadenza.eclipse.service.exchange.ExchangeMappingObjectService;
import net.codecadenza.eclipse.service.java.PackageInfoService;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.operation.DiagramFileCreationOperation;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining project data
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditProjectDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Edit project";

	private Text txtDTONamespace;
	private final Project project;
	private Text txtProjectCode;
	private Text txtDomainNamespace;
	private List listSubNamespaces;
	private Text txtBoundaryNamespace;
	private Text txtSeleniumNamespace;
	private Text txtRepositoryNamespace;
	private Text txtRootNamespace;
	private Text txtExchangeNamespace;
	private Text txtProjectName;
	private Text txtSOAPNamespace;
	private Text txtRESTNamespace;
	private Text txtRMINamespace;
	private Text txtKafkaNamespace;
	private Text txtJMSNamespace;
	private Text txtXmlNamespaceName;
	private Text txtXmlNamespacePrefix;
	private Combo cboMappingStrategy;
	private Combo cboMappingType;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param project
	 */
	public EditProjectDialog(Shell parentShell, Project project) {
		super(parentShell);

		this.project = project;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var gdBasicData = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdBasicData.widthHint = 600;

		final var groupBasicData = new Group(panDialogArea, SWT.NONE);
		groupBasicData.setLayout(new GridLayout(2, false));
		groupBasicData.setLayoutData(gdBasicData);
		groupBasicData.setText("Basic project data");

		final var lblProjectName = new Label(groupBasicData, SWT.LEFT);
		lblProjectName.setText("Project name:");

		txtProjectName = new Text(groupBasicData, SWT.SINGLE | SWT.BORDER);
		txtProjectName.setText(project.getName());
		txtProjectName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtProjectName.setFocus();

		final var lblProjectCode = new Label(groupBasicData, SWT.NONE);
		lblProjectCode.setText("Project code:");

		txtProjectCode = new Text(groupBasicData, SWT.BORDER);
		txtProjectCode.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtProjectCode.setText(project.getCode());

		initNamespaceControls(panDialogArea);

		final var groupXMLBinding = new Group(panDialogArea, SWT.NONE);
		groupXMLBinding.setLayout(new GridLayout(2, false));
		groupXMLBinding.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		groupXMLBinding.setText("XML binding");

		final var lblXmlNamespace = new Label(groupXMLBinding, SWT.NONE);
		lblXmlNamespace.setText("Namespace name:");

		txtXmlNamespaceName = new Text(groupXMLBinding, SWT.BORDER);
		txtXmlNamespaceName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtXmlNamespaceName.setText(project.getXmlNamespace());

		final var lblXmlNamespacePrefix = new Label(groupXMLBinding, SWT.NONE);
		lblXmlNamespacePrefix.setText("Namespace prefix:");

		txtXmlNamespacePrefix = new Text(groupXMLBinding, SWT.BORDER);
		txtXmlNamespacePrefix.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtXmlNamespacePrefix.setText(project.getXmlNamespacePrefix() != null ? project.getXmlNamespacePrefix() : "");

		final var lblMappingStrategy = new Label(groupXMLBinding, SWT.NONE);
		lblMappingStrategy.setText("Mapping strategy:");

		cboMappingStrategy = new Combo(groupXMLBinding, SWT.READ_ONLY);
		cboMappingStrategy.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		for (final MappingAnnotationStrategy strategy : MappingAnnotationStrategy.values())
			cboMappingStrategy.add(strategy.getName());

		int itemIndex = 0;

		for (final String item : cboMappingStrategy.getItems()) {
			if (item.equals(project.getMappingStrategy().name())) {
				cboMappingStrategy.select(itemIndex);
				break;
			}

			itemIndex++;
		}

		final var lblXMLMappingType = new Label(groupXMLBinding, SWT.NONE);
		lblXMLMappingType.setText("Mapping type:");

		cboMappingType = new Combo(groupXMLBinding, SWT.READ_ONLY);
		cboMappingType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		for (final XMLMappingType strategy : XMLMappingType.values())
			cboMappingType.add(strategy.getName());

		itemIndex = 0;

		for (final String item : cboMappingType.getItems()) {
			if (item.equals(project.getDefaultXMLMappingType().name())) {
				cboMappingType.select(itemIndex);
				break;
			}

			itemIndex++;
		}

		return panDialogArea;
	}

	/**
	 * Add the controls for the configuration of the namespaces
	 * @param parent the parent composite
	 */
	private void initNamespaceControls(Composite parent) {
		final var groupNamespaceConf = new Group(parent, SWT.NONE);
		groupNamespaceConf.setLayout(new GridLayout(3, false));
		groupNamespaceConf.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		groupNamespaceConf.setText("Package configuration");

		final var tabFolderNamespaces = new TabFolder(groupNamespaceConf, SWT.NONE);
		tabFolderNamespaces.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

		final var panGeneral = new Composite(tabFolderNamespaces, SWT.NONE);
		panGeneral.setLayout(new GridLayout(2, false));

		final var tabItemGeneral = new TabItem(tabFolderNamespaces, SWT.NONE);
		tabItemGeneral.setText("General");
		tabItemGeneral.setControl(panGeneral);

		final var lblRootNamespace = new Label(panGeneral, SWT.NONE);
		lblRootNamespace.setText("Root package:");

		txtRootNamespace = new Text(panGeneral, SWT.BORDER);
		txtRootNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtRootNamespace.setText(project.getRootNamespace().getName());

		final var lblDomainNamespace = new Label(panGeneral, SWT.NONE);
		lblDomainNamespace.setText("Domain package:");

		txtDomainNamespace = new Text(panGeneral, SWT.BORDER);
		txtDomainNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDomainNamespace.setText(project.getDomainNamespace().getName());

		if (project.isBoundaryMode()) {
			final var lblRepositoryNamespace = new Label(panGeneral, SWT.NONE);
			lblRepositoryNamespace.setText("Repository package:");

			txtRepositoryNamespace = new Text(panGeneral, SWT.BORDER);
			txtRepositoryNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtRepositoryNamespace.setText(project.getRepositoryNamespace().getName());
		}

		final var lblDTONamespace = new Label(panGeneral, SWT.NONE);
		lblDTONamespace.setText("DTO package:");

		txtDTONamespace = new Text(panGeneral, SWT.BORDER);
		txtDTONamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDTONamespace.setText(project.getDTONamespace().getName());

		final var lblExchangeNamespace = new Label(panGeneral, SWT.NONE);
		lblExchangeNamespace.setText("Exchange package:");

		txtExchangeNamespace = new Text(panGeneral, SWT.BORDER);
		txtExchangeNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtExchangeNamespace.setText(project.getExchangeNamespace().getName());

		final var lblBoundaryNamespace = new Label(panGeneral, SWT.NONE);

		if (project.isBoundaryMode())
			lblBoundaryNamespace.setText("Boundary package:");
		else
			lblBoundaryNamespace.setText("Facade package:");

		txtBoundaryNamespace = new Text(panGeneral, SWT.BORDER);
		txtBoundaryNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtBoundaryNamespace.setText(project.getBoundaryNamespace().getName());

		if (project.artifactExists(BuildArtifactType.SELENIUM_TEST)) {
			final String seleniumPackageName = project.getTestModuleByArtifact(BuildArtifactType.SELENIUM_TEST).getNamespace()
					.getName();

			final var lblSeleniumNamespace = new Label(panGeneral, SWT.NONE);
			lblSeleniumNamespace.setText("Selenium package:");

			txtSeleniumNamespace = new Text(panGeneral, SWT.BORDER);
			txtSeleniumNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtSeleniumNamespace.setText(seleniumPackageName);
		}

		final var panSubNamespaces = new Composite(tabFolderNamespaces, SWT.NONE);
		panSubNamespaces.setLayout(new GridLayout(2, false));

		final var tabItemSubNamespaces = new TabItem(tabFolderNamespaces, SWT.NONE);
		tabItemSubNamespaces.setText("Sub-packages");
		tabItemSubNamespaces.setControl(panSubNamespaces);

		listSubNamespaces = new List(panSubNamespaces, SWT.BORDER | SWT.V_SCROLL);
		listSubNamespaces.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		// Add the sub-namespaces to the list
		project.getDomainNamespace().getChildNamespaces().forEach(ns -> listSubNamespaces.add(ns.getName()));

		final var menu = new Menu(listSubNamespaces);
		listSubNamespaces.setMenu(menu);

		final var mnuAdd = new MenuItem(menu, SWT.NONE);
		mnuAdd.setText("Add");

		mnuAdd.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				addSubNamespace();
			}
		});

		final var panButton = new Composite(panSubNamespaces, SWT.NONE);
		panButton.setLayoutData(new GridData(SWT.CENTER, SWT.TOP, false, false));
		panButton.setLayout(new GridLayout());

		final var cmdAdd = new Button(panButton, SWT.NONE);
		cmdAdd.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false));
		cmdAdd.setText("Add");

		cmdAdd.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				addSubNamespace();
			}
		});

		if (!project.getIntegrationModules().isEmpty()) {
			final var panIntegration = new Composite(tabFolderNamespaces, SWT.NONE);
			panIntegration.setLayout(new GridLayout(2, false));

			final var tabItemIntegration = new TabItem(tabFolderNamespaces, SWT.NONE);
			tabItemIntegration.setText("Integration modules");
			tabItemIntegration.setControl(panIntegration);

			for (final IntegrationModule module : project.getIntegrationModules())
				if (module.getTechnology() == IntegrationTechnology.REST) {
					final var lblRESTNamespace = new Label(panIntegration, SWT.NONE);
					lblRESTNamespace.setText("REST package:");

					txtRESTNamespace = new Text(panIntegration, SWT.BORDER);
					txtRESTNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
					txtRESTNamespace.setText(module.getNamespace().getName());
				}
				else if (module.getTechnology() == IntegrationTechnology.SOAP) {
					final var lblSOAPNamespace = new Label(panIntegration, SWT.NONE);
					lblSOAPNamespace.setText("SOAP package:");

					txtSOAPNamespace = new Text(panIntegration, SWT.BORDER);
					txtSOAPNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
					txtSOAPNamespace.setText(module.getNamespace().getName());
				}
				else if (module.getTechnology() == IntegrationTechnology.RMI) {
					final var lblRMINamespace = new Label(panIntegration, SWT.NONE);
					lblRMINamespace.setText("RMI package:");

					txtRMINamespace = new Text(panIntegration, SWT.BORDER);
					txtRMINamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
					txtRMINamespace.setText(module.getNamespace().getName());
				}
				else if (module.getTechnology() == IntegrationTechnology.KAFKA) {
					final var lblKafkaNamespace = new Label(panIntegration, SWT.NONE);
					lblKafkaNamespace.setText("Kafka package:");

					txtKafkaNamespace = new Text(panIntegration, SWT.BORDER);
					txtKafkaNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
					txtKafkaNamespace.setText(module.getNamespace().getName());
				}
				else if (module.getTechnology() == IntegrationTechnology.JMS) {
					final var lblJMSNamespace = new Label(panIntegration, SWT.NONE);
					lblJMSNamespace.setText("JMS package:");

					txtJMSNamespace = new Text(panIntegration, SWT.BORDER);
					txtJMSNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
					txtJMSNamespace.setText(module.getNamespace().getName());
				}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			try {
				if (!validateInput())
					return;

				applyChanges();
			}
			catch (final Exception e) {
				CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
				return;
			}
		}

		super.buttonPressed(buttonId);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText(DLG_TITLE);
	}

	/**
	 * Save changed project data
	 * @throws Exception if either a refactoring or the save operation has failed
	 */
	private void applyChanges() throws Exception {
		boolean rebuildObjectsDependingOnXMLBinding = false;
		var newRepositoryNamespace = "";
		final var rootNamespace = project.getRootNamespace().getName() + ".";
		final var newRootNamespace = txtRootNamespace.getText() + ".";
		final String repositoryNamespace = project.getRepositoryNamespace().toString();
		final String dtoNamespace = rootNamespace + project.getDTONamespace().getName();
		final String newDtoNamespace = newRootNamespace + txtDTONamespace.getText();
		final String domainNamespace = rootNamespace + project.getDomainNamespace().getName();
		final String newDomainNamespace = newRootNamespace + txtDomainNamespace.getText();
		final String boundNamespace = rootNamespace + project.getBoundaryNamespace().getName();
		final String newBoundNamespace = newRootNamespace + txtBoundaryNamespace.getText();
		final String exchangeNamespace = rootNamespace + project.getExchangeNamespace().getName();
		final String newExchangeNamespace = newRootNamespace + txtExchangeNamespace.getText();
		final String clientNamespace = project.getClientNamespace().toString();
		final String newClientNamespace = newRootNamespace + project.getClientNamespace().getName();
		final String serviceNamespace = project.getRootNamespace().toString() + PACK_SERVICE;
		final String newServiceNamespace = txtRootNamespace.getText() + PACK_SERVICE;

		if (project.isBoundaryMode())
			newRepositoryNamespace = newRootNamespace + txtRepositoryNamespace.getText();

		// Rename packages
		for (final BuildArtifact artifact : project.getBuildConfiguration()) {
			if (artifact.getType() == BuildArtifactType.MASTER)
				continue;
			else if (artifact.getType() == BuildArtifactType.SERVER
					&& !project.getRootNamespace().getName().equals(txtRootNamespace.getText()))
				EclipseIDEService.renamePackage(artifact.getName(), project.getSourceFolder(), project.getRootNamespace().getName(),
						txtRootNamespace.getText());

			if (!domainNamespace.equals(newDomainNamespace))
				EclipseIDEService.renamePackage(artifact.getName(), project.getSourceFolder(), domainNamespace, newDomainNamespace);

			if (!dtoNamespace.equals(newDtoNamespace))
				EclipseIDEService.renamePackage(artifact.getName(), project.getSourceFolder(), dtoNamespace, newDtoNamespace);

			if (project.isBoundaryMode() && !repositoryNamespace.equals(newRepositoryNamespace))
				EclipseIDEService.renamePackage(artifact.getName(), project.getSourceFolder(), repositoryNamespace,
						newRepositoryNamespace);

			if (!boundNamespace.equals(newBoundNamespace))
				EclipseIDEService.renamePackage(artifact.getName(), project.getSourceFolder(), boundNamespace, newBoundNamespace);

			if (!exchangeNamespace.equals(newExchangeNamespace))
				EclipseIDEService.renamePackage(artifact.getName(), project.getSourceFolder(), exchangeNamespace, newExchangeNamespace);

			if (!clientNamespace.equals(newClientNamespace))
				EclipseIDEService.renamePackage(artifact.getName(), project.getSourceFolder(), clientNamespace, newClientNamespace);

			if (!serviceNamespace.equals(newServiceNamespace))
				EclipseIDEService.renamePackage(artifact.getName(), project.getSourceFolder(), serviceNamespace, newServiceNamespace);

			for (final IntegrationModule module : project.getIntegrationModules()) {
				final String existingNamespace = module.getNamespace().toString();
				String newNamespace = null;

				if (module.getTechnology() == IntegrationTechnology.REST)
					newNamespace = newRootNamespace + txtRESTNamespace.getText();
				else if (module.getTechnology() == IntegrationTechnology.SOAP)
					newNamespace = newRootNamespace + txtSOAPNamespace.getText();
				else if (module.getTechnology() == IntegrationTechnology.RMI)
					newNamespace = newRootNamespace + txtRMINamespace.getText();
				else if (module.getTechnology() == IntegrationTechnology.KAFKA)
					newNamespace = newRootNamespace + txtKafkaNamespace.getText();
				else if (module.getTechnology() == IntegrationTechnology.JMS)
					newNamespace = newRootNamespace + txtJMSNamespace.getText();

				if (newNamespace != null && !existingNamespace.equals(newNamespace))
					EclipseIDEService.renamePackage(artifact.getName(), project.getSourceFolder(), existingNamespace, newNamespace);
			}
		}

		if (project.artifactExists(BuildArtifactType.SELENIUM_TEST)) {
			final AbstractTestModule seleniumModule = project.getTestModuleByArtifact(BuildArtifactType.SELENIUM_TEST);
			final String seleniumProjectName = project.getTargetProjectName(BuildArtifactType.SELENIUM_TEST);
			final String existingNamespace = rootNamespace + seleniumModule.getNamespace().getName();
			final String newNamespace = newRootNamespace + txtSeleniumNamespace.getText();

			if (!existingNamespace.equals(newNamespace))
				EclipseIDEService.renamePackage(seleniumProjectName, project.getTestSourceFolder(), existingNamespace, newNamespace);

			seleniumModule.getNamespace().setName(txtSeleniumNamespace.getText());
		}

		// Test if objects must be rebuilt that depend on the global XML binding configuration
		if (!project.getXmlNamespace().equals(txtXmlNamespaceName.getText())
				|| !txtXmlNamespacePrefix.getText().equals(project.getXmlNamespacePrefix()))
			rebuildObjectsDependingOnXMLBinding = true;

		if (project.getDefaultXMLMappingType() != XMLMappingType.valueOf(cboMappingType.getItem(cboMappingType.getSelectionIndex())))
			rebuildObjectsDependingOnXMLBinding = true;

		if (project.getMappingStrategy() != MappingAnnotationStrategy
				.valueOf(cboMappingStrategy.getItem(cboMappingStrategy.getSelectionIndex())))
			rebuildObjectsDependingOnXMLBinding = true;

		// Update project data
		project.setCode(txtProjectCode.getText());
		project.setName(txtProjectName.getText());
		project.setXmlNamespace(txtXmlNamespaceName.getText());
		project.setXmlNamespacePrefix(txtXmlNamespacePrefix.getText());
		project.setDefaultXMLMappingType(XMLMappingType.valueOf(cboMappingType.getItem(cboMappingType.getSelectionIndex())));
		project.setMappingStrategy(
				MappingAnnotationStrategy.valueOf(cboMappingStrategy.getItem(cboMappingStrategy.getSelectionIndex())));
		project.getRootNamespace().setName(txtRootNamespace.getText());
		project.getDTONamespace().setName(txtDTONamespace.getText());
		project.getDomainNamespace().setName(txtDomainNamespace.getText());
		project.getBoundaryNamespace().setName(txtBoundaryNamespace.getText());
		project.getExchangeNamespace().setName(txtExchangeNamespace.getText());

		if (project.isBoundaryMode())
			project.getRepositoryNamespace().setName(txtRepositoryNamespace.getText());

		// Change integration module namespace names
		project.getIntegrationModules().forEach(module -> {
			final String existingNamespace = module.getNamespace().toString();
			String newNamespace = null;

			if (module.getTechnology() == IntegrationTechnology.REST)
				newNamespace = project.getRootNamespace().toString() + "." + txtRESTNamespace.getText();
			else if (module.getTechnology() == IntegrationTechnology.SOAP)
				newNamespace = project.getRootNamespace().toString() + "." + txtSOAPNamespace.getText();
			else if (module.getTechnology() == IntegrationTechnology.RMI)
				newNamespace = project.getRootNamespace().toString() + "." + txtRMINamespace.getText();
			else if (module.getTechnology() == IntegrationTechnology.KAFKA)
				newNamespace = project.getRootNamespace().toString() + "." + txtKafkaNamespace.getText();
			else if (module.getTechnology() == IntegrationTechnology.JMS)
				newNamespace = project.getRootNamespace().toString() + "." + txtJMSNamespace.getText();

			if (newNamespace != null && !existingNamespace.equals(newNamespace)) {
				if (module.getTechnology() == IntegrationTechnology.REST)
					module.getNamespace().setName(txtRESTNamespace.getText());
				else if (module.getTechnology() == IntegrationTechnology.SOAP)
					module.getNamespace().setName(txtSOAPNamespace.getText());
				else if (module.getTechnology() == IntegrationTechnology.RMI)
					module.getNamespace().setName(txtRMINamespace.getText());
				else if (module.getTechnology() == IntegrationTechnology.KAFKA)
					module.getNamespace().setName(txtKafkaNamespace.getText());
				else if (module.getTechnology() == IntegrationTechnology.JMS)
					module.getNamespace().setName(txtJMSNamespace.getText());
			}
		});

		// Save project meta-data
		EclipseIDEService.saveProjectMetaData(project);

		// Rebuild objects that depend on the global XML binding configuration
		if (rebuildObjectsDependingOnXMLBinding)
			rebuildObjectsDependingOnXMLBinding();
	}

	/**
	 * @return true if the validation was successful
	 */
	private boolean validateInput() {
		// Check if the user has provided a valid project name
		final IWorkspace workspace = ResourcesPlugin.getWorkspace();
		final IStatus status = workspace.validateName(txtProjectName.getText(), IResource.PROJECT);

		if (!status.isOK()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The project name is not valid!");
			txtProjectName.setFocus();
			return false;
		}

		// Validate if a project code has been entered
		if (txtProjectCode.getText().isEmpty()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The project code must not be empty!");
			txtProjectCode.setFocus();
			return false;
		}

		// Validate the root namespace
		if (!validatePackageNameOfControl(txtRootNamespace))
			return false;

		// Validate the repository namespace
		if (project.isBoundaryMode() && !validatePackageNameOfControl(txtRepositoryNamespace))
			return false;

		// Validate the DTO namespace
		if (!validatePackageNameOfControl(txtDTONamespace))
			return false;

		// Validate the domain namespace
		if (!validatePackageNameOfControl(txtDomainNamespace))
			return false;

		// Validate the exchange namespace
		if (!validatePackageNameOfControl(txtExchangeNamespace))
			return false;

		// Validate the boundary namespace
		if (!validatePackageNameOfControl(txtBoundaryNamespace))
			return false;

		// Validate the Selenium namespace
		if (txtSeleniumNamespace != null && !validatePackageNameOfControl(txtSeleniumNamespace))
			return false;

		// Validate the integration namespaces
		for (final IntegrationModule module : project.getIntegrationModules())
			if ((module.getTechnology() == IntegrationTechnology.REST && !validatePackageNameOfControl(txtRESTNamespace))
					|| (module.getTechnology() == IntegrationTechnology.SOAP && !validatePackageNameOfControl(txtSOAPNamespace))
					|| (module.getTechnology() == IntegrationTechnology.RMI && !validatePackageNameOfControl(txtRMINamespace))
					|| (module.getTechnology() == IntegrationTechnology.KAFKA && !validatePackageNameOfControl(txtKafkaNamespace))
					|| (module.getTechnology() == IntegrationTechnology.JMS && !validatePackageNameOfControl(txtJMSNamespace)))
				return false;

		// A XML namespace name is mandatory!
		if (txtXmlNamespaceName.getText().isEmpty()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The XML namespace name must not be empty!");
			txtXmlNamespaceName.setFocus();
			return false;
		}

		return true;
	}

	/**
	 * Rebuild all objects that depend on the global XML binding configuration
	 * @throws Exception if a rebuild operation of a given object has failed
	 */
	private void rebuildObjectsDependingOnXMLBinding() throws Exception {
		final var dataExchangeBeanService = new DataExchangeBeanService(project);
		final var exchangeMappingObjectService = new ExchangeMappingObjectService();

		// Rebuild all exchange mapping objects and XML schema files
		for (final Namespace ns : project.getExchangeNamespace().getChildNamespaces())
			for (final JavaType type : ns.getJavaTypes()) {
				if (type instanceof final DataExchangeServiceBean exchangeService)
					for (final DataExchangeMethod exchangeMethod : exchangeService.getDataExchangeMethods())
						dataExchangeBeanService.rebuildSchemaFile(exchangeMethod);
				else if (type instanceof final ExchangeMappingObject exchangeMappingObject)
					exchangeMappingObjectService.rebuildExchangeMappingObject(exchangeMappingObject);
			}

		// Rebuild all data transfer objects
		for (final Namespace ns : project.getDTONamespace().getChildNamespaces())
			ProjectBuildFactory.getBuildService(project).buildObjectsOfNamespace(ns, false, new NullProgressMonitor());

		// Rebuild all SOAP services
		final IntegrationModule soapModule = project.getIntegrationModuleByArtifact(BuildArtifactType.INTEGRATION_IMP_SOAP);

		if (soapModule != null)
			ProjectBuildFactory.getBuildService(project).buildObjectsOfNamespace(soapModule.getNamespace(), false,
					new NullProgressMonitor());

		// Rebuild or remove all package-info files
		new PackageInfoService(project).rebuildPackageInfoFiles();
	}

	/**
	 * Validate the package name based on the entered text of the given control
	 * @param txtPackageControl
	 * @return true if the validation of the package name was successful
	 */
	private boolean validatePackageNameOfControl(Text txtPackageControl) {
		final IStatus status = EclipseIDEService.validatePackageName(txtPackageControl.getText());

		if (status.getSeverity() > IStatus.INFO) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, status.getMessage());
			txtPackageControl.setFocus();
			return false;
		}

		return true;
	}

	/**
	 * Add a new sub-namespace to the project
	 */
	private void addSubNamespace() {
		final var dlg = new InputDialog(getShell(), DLG_TITLE, "Enter the name of a new sub-package:", "", null);

		if (dlg.open() != Window.OK)
			return;

		for (final String item : listSubNamespaces.getItems())
			if (item.equals(dlg.getValue())) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "A sub-package with the name '" + item + "' already exists!");
				return;
			}

		final IStatus status = EclipseIDEService.validatePackageName(dlg.getValue());

		if (status.getSeverity() > IStatus.INFO) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, status.getMessage());
			return;
		}

		listSubNamespaces.add(dlg.getValue());

		final Resource resource = project.eResource();

		final Namespace repositorySubNamespace = JavaFactory.eINSTANCE.createNamespace();
		repositorySubNamespace.setName(dlg.getValue());
		repositorySubNamespace.setParent(project.getRepositoryNamespace());
		repositorySubNamespace.setProject(project);

		project.getRepositoryNamespace().getChildNamespaces().add(repositorySubNamespace);
		resource.getContents().add(repositorySubNamespace);

		final Namespace boundarySubNamespace = JavaFactory.eINSTANCE.createNamespace();
		boundarySubNamespace.setName(dlg.getValue());
		boundarySubNamespace.setParent(project.getBoundaryNamespace());
		boundarySubNamespace.setProject(project);

		project.getBoundaryNamespace().getChildNamespaces().add(boundarySubNamespace);
		resource.getContents().add(boundarySubNamespace);

		final Namespace dtoSubNamespace = JavaFactory.eINSTANCE.createNamespace();
		dtoSubNamespace.setName(dlg.getValue());
		dtoSubNamespace.setParent(project.getDTONamespace());
		dtoSubNamespace.setProject(project);

		project.getDTONamespace().getChildNamespaces().add(dtoSubNamespace);
		resource.getContents().add(dtoSubNamespace);

		final Namespace exchangeSubNamespace = JavaFactory.eINSTANCE.createNamespace();
		exchangeSubNamespace.setName(dlg.getValue());
		exchangeSubNamespace.setParent(project.getExchangeNamespace());
		exchangeSubNamespace.setProject(project);

		project.getExchangeNamespace().getChildNamespaces().add(exchangeSubNamespace);
		resource.getContents().add(exchangeSubNamespace);

		final DomainNamespace subDomainObjNamespace = DomainFactory.eINSTANCE.createDomainNamespace();
		subDomainObjNamespace.setName(dlg.getValue());
		subDomainObjNamespace.setParent(project.getDomainNamespace());
		subDomainObjNamespace.setProject(project);

		project.getDomainNamespace().getChildNamespaces().add(subDomainObjNamespace);

		final String listenerPackage = subDomainObjNamespace.toString() + SUB_PACKAGE_LISTENER;

		try {
			// Create a namespace resource file
			final var path = MODEL_FOLDER + "/package-" + dlg.getValue() + "." + MODEL_FILE_EXTENSION;
			final var modelFile = new WorkspaceFile(project, BuildArtifactType.DOMAIN, path, "");
			final IFile namesSpaceFile = EclipseIDEService.createOrUpdateFile(modelFile);
			final URI namespaceURI = URI.createFileURI(namesSpaceFile.getLocationURI().getPath());

			// Create a resource
			final Resource namespaceResource = project.eResource().getResourceSet().createResource(namespaceURI);

			// Add the namespace
			namespaceResource.getContents().add(subDomainObjNamespace);

			// Save the namespace resource
			EclipseIDEService.saveProjectMetaData(namespaceResource, project);

			// Save the project
			EclipseIDEService.saveProjectMetaData(project);

			// Create the respective domain diagram
			DiagramFileCreationOperation.initializeAndOpenDomainDiagram(project, namespaceURI);

			// Create packages in the workspace
			final String sourceFolder = project.getSourceFolder();

			String projectName = project.getTargetProjectName(BuildArtifactType.DOMAIN);
			EclipseIDEService.createPackage(projectName, sourceFolder, subDomainObjNamespace.toString());
			EclipseIDEService.createPackage(projectName, sourceFolder, listenerPackage);

			projectName = project.getTargetProjectName(BuildArtifactType.DTO);
			EclipseIDEService.createPackage(projectName, sourceFolder, dtoSubNamespace.toString());

			projectName = project.getTargetProjectName(BuildArtifactType.SHARED);
			EclipseIDEService.createPackage(projectName, sourceFolder, subDomainObjNamespace.toString());

			projectName = project.getTargetProjectName(BuildArtifactType.DATA_EXCHANGE);
			EclipseIDEService.createPackage(projectName, sourceFolder, exchangeSubNamespace.toString());

			if (project.isBoundaryMode()) {
				projectName = project.getTargetProjectName(BuildArtifactType.REPOSITORY);
				EclipseIDEService.createPackage(projectName, sourceFolder, repositorySubNamespace.toString());

				projectName = project.getTargetProjectName(BuildArtifactType.BOUNDARY);

				if (project.isAddBoundaryInterface()) {
					EclipseIDEService.createPackage(projectName, sourceFolder, boundarySubNamespace.toString() + SUB_PACKAGE_BEAN);

					projectName = project.getTargetProjectName(BuildArtifactType.CLIENT_INTERFACE);
				}
			}
			else
				projectName = project.getTargetProjectName(BuildArtifactType.FACADE);

			EclipseIDEService.createPackage(projectName, sourceFolder, boundarySubNamespace.toString());
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
		}
	}

}
