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
package net.codecadenza.eclipse.ui.wizard;

import static net.codecadenza.eclipse.shared.Constants.ANGULAR_ARTIFACT_REGEX;
import static net.codecadenza.eclipse.shared.Constants.JAVA_ARTIFACT_REGEX;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.BuildToolEnumeration;
import net.codecadenza.eclipse.model.project.ClientPlatformEnumeration;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ProjectFactory;
import net.codecadenza.eclipse.service.build.ProjectBuildFactory;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

/**
 * <p>
 * Wizard page for the build configuration
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BuildConfigWizardPage extends WizardPage {
	private Combo cboBuildTool;
	private TableViewer tableViewer;
	private IStatus currentStatus;
	private boolean pageVisible;
	private final ProjectWizardPage pageProject;
	private final ApplicationWizardPage pageApp;
	private final IntegrationWizardPage pageIntegration;
	private final TestingWizardPage pageTesting;
	private BuildToolEnumeration buildTool;
	private List<BuildArtifact> buildConfiguration;

	/**
	 * Constructor
	 * @param pageNumber
	 * @param pageProject
	 * @param pageApp
	 * @param pageIntegration
	 * @param pageTesting
	 */
	public BuildConfigWizardPage(int pageNumber, ProjectWizardPage pageProject, ApplicationWizardPage pageApp,
			IntegrationWizardPage pageIntegration, TestingWizardPage pageTesting) {
		super("page" + pageNumber);

		this.currentStatus = createStatus(IStatus.OK, "");
		this.pageApp = pageApp;
		this.pageProject = pageProject;
		this.pageIntegration = pageIntegration;
		this.pageTesting = pageTesting;

		setTitle("Build configuration");
		setDescription("Select a build tool...");

		// Mark the page as incomplete in order to disable the wizard's 'Finish' button!
		setPageComplete(false);
	}

	/**
	 * Refresh the page
	 */
	private void refreshPage() {
		if (cboBuildTool.getItemCount() == 0) {
			// The wizard must not be finished if no build tool is available!
			updateStatus(createStatus(IStatus.ERROR, "No appropriate build tool available!"));
			return;
		}

		buildTool = BuildToolEnumeration.valueOf(cboBuildTool.getItem(cboBuildTool.getSelectionIndex()));

		// Create a temporary project instance and fill it with required data
		final Project project = ProjectFactory.eINSTANCE.createProject();
		project.setBoundaryMode(true);
		project.setBuildTool(buildTool);
		project.setCode(pageProject.getProjectCode());
		project.setName(pageProject.getName());
		project.setClientPlatform(pageApp.getClientPlatform());
		project.setTechnology(pageApp.getTechnologyPlatform());
		project.getIntegrationModules().addAll(pageIntegration.getIntegrationModules());

		if (!pageProject.isBoundaryMode() && pageApp.isFacadeModeSupported())
			project.setBoundaryMode(false);

		buildConfiguration = ProjectBuildFactory.getBuildService(project).getDefaultBuildConfiguration();

		// The default build configuration contains all possible artifacts. If they should not be generated we must remove them here!
		final List<BuildArtifact> artifactList = new ArrayList<>(buildConfiguration);

		artifactList.forEach(artifact -> {
			final BuildArtifactType artifactType = artifact.getType();
			boolean remove = false;

			if (!pageIntegration.isAddSOAPModule()) {
				if (artifactType == BuildArtifactType.INTEGRATION_CLIENT_SOAP || artifactType == BuildArtifactType.INTEGRATION_IMP_SOAP
						|| artifactType == BuildArtifactType.INTEGRATION_SEI_SOAP)
					remove = true;
			}
			else if (!pageIntegration.isAddSOAPClient() && artifactType == BuildArtifactType.INTEGRATION_CLIENT_SOAP)
				remove = true;

			if (!pageIntegration.isAddRESTModule()) {
				if (artifactType == BuildArtifactType.INTEGRATION_CLIENT_REST || artifactType == BuildArtifactType.INTEGRATION_IMP_REST
						|| artifactType == BuildArtifactType.INTEGRATION_SEI_REST)
					remove = true;
			}
			else if (!pageIntegration.isAddRESTClient() && (artifactType == BuildArtifactType.INTEGRATION_CLIENT_REST
					|| artifactType == BuildArtifactType.INTEGRATION_SEI_REST))
				remove = true;

			if (!pageIntegration.isAddRMIModule()) {
				if (artifactType == BuildArtifactType.INTEGRATION_CLIENT_RMI || artifactType == BuildArtifactType.INTEGRATION_IMP_RMI
						|| artifactType == BuildArtifactType.INTEGRATION_SEI_RMI)
					remove = true;
			}
			else if (!pageIntegration.isAddRMIClient() && artifactType == BuildArtifactType.INTEGRATION_CLIENT_RMI)
				remove = true;

			if (!pageIntegration.isAddKafkaModule()) {
				if (artifactType == BuildArtifactType.INTEGRATION_CLIENT_KAFKA || artifactType == BuildArtifactType.INTEGRATION_IMP_KAFKA
						|| artifactType == BuildArtifactType.INTEGRATION_SEI_KAFKA)
					remove = true;
			}
			else if (!pageIntegration.isAddKafkaClient() && artifactType == BuildArtifactType.INTEGRATION_CLIENT_KAFKA)
				remove = true;

			if (!pageIntegration.isAddJMSModule()) {
				if (artifactType == BuildArtifactType.INTEGRATION_CLIENT_JMS || artifactType == BuildArtifactType.INTEGRATION_IMP_JMS
						|| artifactType == BuildArtifactType.INTEGRATION_SEI_JMS)
					remove = true;
			}
			else if (!pageIntegration.isAddJMSClient()
					&& (artifactType == BuildArtifactType.INTEGRATION_CLIENT_JMS || artifactType == BuildArtifactType.INTEGRATION_SEI_JMS))
				remove = true;

			if (!pageTesting.isAddSeleniumTestModule() && artifactType == BuildArtifactType.SELENIUM_TEST)
				remove = true;

			if (remove)
				buildConfiguration.remove(artifact);
		});

		// Remove the contained integration build artifacts!
		artifactList.forEach(artifact -> {
			if (!pageIntegration.isAddSOAPModule()) {
				artifact.getContainedArtifacts().remove(BuildArtifactType.INTEGRATION_IMP_SOAP);
				artifact.getContainedArtifacts().remove(BuildArtifactType.INTEGRATION_SEI_SOAP);
			}

			if (!pageIntegration.isAddRESTModule()) {
				artifact.getContainedArtifacts().remove(BuildArtifactType.INTEGRATION_IMP_REST);
				artifact.getContainedArtifacts().remove(BuildArtifactType.INTEGRATION_SEI_REST);
			}

			if (!pageIntegration.isAddRMIModule()) {
				artifact.getContainedArtifacts().remove(BuildArtifactType.INTEGRATION_IMP_RMI);
				artifact.getContainedArtifacts().remove(BuildArtifactType.INTEGRATION_SEI_RMI);
			}

			if (!pageIntegration.isAddKafkaModule()) {
				artifact.getContainedArtifacts().remove(BuildArtifactType.INTEGRATION_IMP_KAFKA);
				artifact.getContainedArtifacts().remove(BuildArtifactType.INTEGRATION_SEI_KAFKA);
			}

			if (!pageIntegration.isAddJMSModule()) {
				artifact.getContainedArtifacts().remove(BuildArtifactType.INTEGRATION_IMP_JMS);
				artifact.getContainedArtifacts().remove(BuildArtifactType.INTEGRATION_SEI_JMS);
			}
		});

		tableViewer.setInput(buildConfiguration);

		validateArtifactNames();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createControl(Composite parent) {
		final var panPageArea = new Composite(parent, SWT.NONE);
		panPageArea.setLayout(new GridLayout(2, false));

		final var lblBuildTool = new Label(panPageArea, SWT.NONE);
		lblBuildTool.setText("Build tool:");

		cboBuildTool = new Combo(panPageArea, SWT.NONE | SWT.READ_ONLY);
		cboBuildTool.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		cboBuildTool.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				refreshPage();
			}
		});

		final var lblProperties = new Label(panPageArea, SWT.NONE);
		lblProperties.setText("Build artifacts:");

		new Label(panPageArea, SWT.NONE);

		tableViewer = new TableViewer(panPageArea, SWT.BORDER | SWT.FULL_SELECTION);
		tableViewer.setContentProvider(new ContentProvider());
		tableViewer.getTable().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1));
		tableViewer.getTable().setLinesVisible(true);
		tableViewer.getTable().setHeaderVisible(true);

		final var colName = new TableViewerColumn(tableViewer, SWT.NONE);
		colName.setEditingSupport(new ArtifactNameEditingSupport(tableViewer));
		colName.getColumn().setWidth(150);
		colName.getColumn().setText("Name");

		colName.setLabelProvider(new ColumnLabelProvider() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.viewers.ColumnLabelProvider#getText(java.lang.Object)
			 */
			@Override
			public String getText(Object element) {
				return ((BuildArtifact) element).getName();
			}
		});

		final var colType = new TableViewerColumn(tableViewer, SWT.NONE);
		colType.getColumn().setWidth(180);
		colType.getColumn().setText("Type");

		colType.setLabelProvider(new ColumnLabelProvider() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.viewers.ColumnLabelProvider#getText(java.lang.Object)
			 */
			@Override
			public String getText(Object element) {
				return ((BuildArtifact) element).getType().getName();
			}
		});

		final var colContained = new TableViewerColumn(tableViewer, SWT.NONE);
		colContained.getColumn().setWidth(300);
		colContained.getColumn().setText("Contained artifacts");

		colContained.setLabelProvider(new ColumnLabelProvider() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.viewers.ColumnLabelProvider#getText(java.lang.Object)
			 */
			@Override
			public String getText(Object element) {
				var containedArtifacts = "";

				for (final BuildArtifactType artifact : ((BuildArtifact) element).getContainedArtifacts()) {
					if (!containedArtifacts.isEmpty())
						containedArtifacts += ", ";

					containedArtifacts += artifact.getName();
				}

				return containedArtifacts;
			}
		});

		setControl(panPageArea);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.DialogPage#setVisible(boolean)
	 */
	@Override
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		pageVisible = visible;

		if (visible) {
			cboBuildTool.removeAll();

			// Add all available build tools
			for (final BuildToolEnumeration bt : BuildToolEnumeration.values()) {
				if (bt == BuildToolEnumeration.MAVEN && !EclipseIDEService.isM2EInstalled())
					continue;

				cboBuildTool.add(bt.getName());
			}

			// Preselect a build tool
			if (cboBuildTool.getItemCount() > 0)
				cboBuildTool.select(0);

			refreshPage();
		}

		updateStatus(currentStatus);
	}

	/**
	 * Update the status line and the 'Finish' button depending on the status
	 * @param status
	 */
	private void updateStatus(IStatus status) {
		currentStatus = status;
		setPageComplete(!status.matches(IStatus.ERROR));

		if (pageVisible)
			applyToStatusLine(this, status);
	}

	/**
	 * @param page
	 * @param status
	 */
	private static void applyToStatusLine(DialogPage page, IStatus status) {
		String errorMessage = null;
		String warningMessage = null;
		final String statusMessage = status.getMessage();

		if (statusMessage.length() > 0) {
			if (status.matches(IStatus.ERROR))
				errorMessage = statusMessage;
			else if (!status.isOK())
				warningMessage = statusMessage;
		}

		page.setErrorMessage(errorMessage);
		page.setMessage(warningMessage);
	}

	/**
	 * Content provider for the table viewer
	 */
	protected class ContentProvider implements IStructuredContentProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Object[] getElements(Object inputElement) {
			return ((Collection<BuildArtifact>) inputElement).toArray();
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#dispose()
		 */
		@Override
		public void dispose() {
			// No implementation required!
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			// No implementation required!
		}
	}

	/**
	 * Editing support for the artifact name column
	 */
	public class ArtifactNameEditingSupport extends EditingSupport {
		private final TableViewer viewer;
		private final CellEditor editor;

		/**
		 * @param viewer
		 */
		public ArtifactNameEditingSupport(TableViewer viewer) {
			super(viewer);

			this.viewer = viewer;
			this.editor = new TextCellEditor(viewer.getTable());
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.EditingSupport#getCellEditor(java.lang.Object)
		 */
		@Override
		protected CellEditor getCellEditor(Object element) {
			return editor;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.EditingSupport#canEdit(java.lang.Object)
		 */
		@Override
		protected boolean canEdit(Object element) {
			return true;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.EditingSupport#getValue(java.lang.Object)
		 */
		@Override
		protected Object getValue(Object element) {
			return ((BuildArtifact) element).getName();
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.EditingSupport#setValue(java.lang.Object, java.lang.Object)
		 */
		@Override
		protected void setValue(Object element, Object userInputValue) {
			final String newArtifactName = String.valueOf(userInputValue);

			((BuildArtifact) element).setName(newArtifactName);

			viewer.update(element, null);

			// We must check all names in order to avoid loosing a possible error status of another artifact!
			validateArtifactNames();
		}
	}

	/**
	 * Validate all artifact names
	 */
	private void validateArtifactNames() {
		for (final BuildArtifact artifact : buildConfiguration) {
			final IStatus status = validateArtifactName(artifact, artifact.getName());

			updateStatus(status);

			if (status.matches(IStatus.ERROR))
				return;
		}
	}

	/**
	 * Validate the name of the given artifact
	 * @param buildArtifact
	 * @param name
	 * @return the validation status
	 */
	private IStatus validateArtifactName(BuildArtifact buildArtifact, String name) {
		final IWorkspace workspace = ResourcesPlugin.getWorkspace();
		String regex = JAVA_ARTIFACT_REGEX;

		if (buildArtifact.getType() == BuildArtifactType.GUI && pageApp.getClientPlatform() == ClientPlatformEnumeration.ANGULAR)
			regex = ANGULAR_ARTIFACT_REGEX;

		if (name.isEmpty() || !name.matches(regex))
			return createStatus(IStatus.ERROR, "The artifact name '" + name + "' is not valid!");

		for (final BuildArtifact artifact : buildConfiguration)
			if (buildArtifact != artifact && name.equals(artifact.getName()))
				return createStatus(IStatus.ERROR, "Artifact names must be unique!");

		// Test if a project with the same name already exists
		if (workspace.getRoot().getProject(name).exists())
			return createStatus(IStatus.ERROR, "A project with the name '" + name + "' already exists!");

		return createStatus(IStatus.INFO, "");
	}

	/**
	 * @param severity
	 * @param message
	 * @return the status
	 */
	private static IStatus createStatus(int severity, String message) {
		return new Status(severity, CodeCadenzaUserInterfacePlugin.PLUGIN_ID, severity, message, null);
	}

	/**
	 * @return the selected build tool
	 */
	public BuildToolEnumeration getBuildTool() {
		return buildTool;
	}

	/**
	 * @return the build configuration
	 */
	public List<BuildArtifact> getBuildConfiguration() {
		return buildConfiguration;
	}

}
