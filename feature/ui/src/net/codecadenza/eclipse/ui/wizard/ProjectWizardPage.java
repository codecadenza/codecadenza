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

import static net.codecadenza.eclipse.shared.Constants.DEFAULT_SUB_PACKAGE;
import static net.codecadenza.eclipse.shared.Constants.JAVA_ARTIFACT_REGEX;
import static net.codecadenza.eclipse.shared.Constants.PREF_BOUNDARY_CONTEXT;
import static net.codecadenza.eclipse.shared.Constants.PREF_DOMAIN_CONTEXT;
import static net.codecadenza.eclipse.shared.Constants.PREF_DTO_CONTEXT;
import static net.codecadenza.eclipse.shared.Constants.PREF_EXCHANGE_CONTEXT;
import static net.codecadenza.eclipse.shared.Constants.PREF_FACADE_CONTEXT;
import static net.codecadenza.eclipse.shared.Constants.PREF_REPOSITORY_CONTEXT;
import static net.codecadenza.eclipse.shared.Constants.PREF_ROOT_CONTEXT;

import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.emf.common.util.URI;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Project wizard page
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ProjectWizardPage extends WizardPage {
	private static final String DEFAULT_PROJECT_CODE = "code";
	private static final String DEFAULT_PROJECT_NAME = "NewCodeCadenzaProject";
	private static final Pattern ARTIFACT_NAME_PATTERN = Pattern.compile(JAVA_ARTIFACT_REGEX);

	private List listSubNamespaces;
	private Text txtRepositoryNamespace;
	private Text txtBoundaryNamespace;
	private String dtoNamespaceName = "";
	private String exchangeNamespaceName = "";
	private IStatus currentStatus;
	private boolean pageVisible;
	private String repositoryNamespaceName = "";
	private String rootNamespaceName = "";
	private String boundaryNamespaceName = "";
	private String domainNamespaceName = "";
	private String projectName = "";
	private String projectCode = "";
	private final Set<URI> modelURISet = new HashSet<>();
	private Project project;
	private boolean boundaryMode = true;
	private Button chkBoundaryMode;
	private Label lblBoundaryNamespace;
	private Label lblRepositoryNamespace;
	private boolean protectManualChanges = true;
	private Button chkProtectManualChanges;

	/**
	 * Constructor
	 * @param pageNumber
	 */
	public ProjectWizardPage(int pageNumber) {
		super("page" + pageNumber);

		this.currentStatus = createStatus(IStatus.OK, "");
		this.projectName = DEFAULT_PROJECT_NAME;

		setTitle("CodeCadenza project settings");
		setDescription("Enter project specific settings");
	}

	/**
	 * @return a set containing all model URIs
	 */
	public Set<URI> getModelURISet() {
		return modelURISet;
	}

	/**
	 * @return the project
	 */
	public Project getProject() {
		return project;
	}

	/**
	 * @param project
	 */
	public void setProject(Project project) {
		this.project = project;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createControl(Composite parent) {
		// Get the preference store
		final IPreferenceStore store = CodeCadenzaUserInterfacePlugin.getInstance().getPreferenceStore();

		final var panPageArea = new Composite(parent, SWT.NONE);
		panPageArea.setLayout(new GridLayout());

		setControl(panPageArea);

		final var groupBasicData = new Group(panPageArea, SWT.NONE);
		groupBasicData.setText("Basic data");
		groupBasicData.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		groupBasicData.setLayout(new GridLayout(2, false));

		final var label = new Label(groupBasicData, SWT.LEFT);
		label.setText("Project name:");

		final var txtProjectName = new Text(groupBasicData, SWT.SINGLE | SWT.BORDER);
		txtProjectName.setText(projectName);
		txtProjectName.setSelection(projectName.length());
		txtProjectName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtProjectName.setFocus();

		txtProjectName.addModifyListener(e -> {
			if (!txtProjectName.isDisposed())
				validateProject(txtProjectName.getText());
		});

		final var lblProjectCode = new Label(groupBasicData, SWT.NONE);
		lblProjectCode.setText("Project code:");

		final var txtProjectCode = new Text(groupBasicData, SWT.BORDER);
		txtProjectCode.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtProjectCode.setText(DEFAULT_PROJECT_CODE);

		txtProjectCode.addModifyListener(e -> {
			if (!txtProjectCode.isDisposed())
				validateProjectCode(txtProjectCode.getText());
		});

		projectCode = txtProjectCode.getText();

		final var lblBoundaryMode = new Label(groupBasicData, SWT.NONE);
		lblBoundaryMode.setText("Boundary mode:");

		chkBoundaryMode = new Button(groupBasicData, SWT.CHECK);
		chkBoundaryMode.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
		chkBoundaryMode.setSelection(true);

		chkBoundaryMode.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				boundaryMode = chkBoundaryMode.getSelection();

				if (boundaryMode) {
					lblBoundaryNamespace.setText("Boundary package:");
					txtBoundaryNamespace.setText(store.getString(PREF_BOUNDARY_CONTEXT));
					txtRepositoryNamespace.setEnabled(true);
					lblRepositoryNamespace.setEnabled(true);
				}
				else {
					lblBoundaryNamespace.setText("Facade service package:");
					txtBoundaryNamespace.setText(store.getString(PREF_FACADE_CONTEXT));
					txtRepositoryNamespace.setEnabled(false);
					lblRepositoryNamespace.setEnabled(false);
				}
			}
		});

		final var lblProtectManualChanges = new Label(groupBasicData, SWT.NONE);
		lblProtectManualChanges.setText("Protect manual changes:");

		chkProtectManualChanges = new Button(groupBasicData, SWT.CHECK);
		chkProtectManualChanges.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
		chkProtectManualChanges.setSelection(true);

		chkProtectManualChanges.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				protectManualChanges = chkProtectManualChanges.getSelection();
			}
		});

		final var groupNamespaceConf = new Group(panPageArea, SWT.NONE);
		groupNamespaceConf.setText("Package configuration");
		groupNamespaceConf.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		groupNamespaceConf.setLayout(new GridLayout(2, false));

		final var lblRootNamespace = new Label(groupNamespaceConf, SWT.NONE);
		lblRootNamespace.setText("Root package:");

		final var txtRootNamespace = new Text(groupNamespaceConf, SWT.BORDER);
		txtRootNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtRootNamespace.setText(store.getString(PREF_ROOT_CONTEXT));
		txtRootNamespace.addModifyListener(e -> validateRootNamespaceName(txtRootNamespace.getText()));

		rootNamespaceName = txtRootNamespace.getText();

		final var lblDomainNamespace = new Label(groupNamespaceConf, SWT.NONE);
		lblDomainNamespace.setText("Domain package:");

		final var txtDomainNamespace = new Text(groupNamespaceConf, SWT.BORDER);
		txtDomainNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDomainNamespace.setText(store.getString(PREF_DOMAIN_CONTEXT));
		txtDomainNamespace.addModifyListener(e -> validateDomainNamespace(txtDomainNamespace.getText()));

		domainNamespaceName = txtDomainNamespace.getText();

		final var lblDTONamespace = new Label(groupNamespaceConf, SWT.NONE);
		lblDTONamespace.setText("DTO package:");

		final var txtDTONamespace = new Text(groupNamespaceConf, SWT.BORDER);
		txtDTONamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDTONamespace.setText(store.getString(PREF_DTO_CONTEXT));
		txtDTONamespace.addModifyListener(e -> validateDTONamespaceName(txtDTONamespace.getText()));

		dtoNamespaceName = txtDTONamespace.getText();

		final var lblExchangeNamespace = new Label(groupNamespaceConf, SWT.NONE);
		lblExchangeNamespace.setText("Exchange package:");

		final var txtExchangeNamespace = new Text(groupNamespaceConf, SWT.BORDER);
		txtExchangeNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtExchangeNamespace.setText(store.getString(PREF_EXCHANGE_CONTEXT));
		txtExchangeNamespace.addModifyListener(e -> validateExchangeNamespaceName(txtExchangeNamespace.getText()));

		exchangeNamespaceName = txtExchangeNamespace.getText();

		lblBoundaryNamespace = new Label(groupNamespaceConf, SWT.NONE);
		lblBoundaryNamespace.setText("Boundary service package:");

		txtBoundaryNamespace = new Text(groupNamespaceConf, SWT.BORDER);
		txtBoundaryNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtBoundaryNamespace.setText(store.getString(PREF_BOUNDARY_CONTEXT));
		txtBoundaryNamespace.addModifyListener(e -> validateBoundaryNamespaceName(txtBoundaryNamespace.getText()));

		boundaryNamespaceName = txtBoundaryNamespace.getText();

		lblRepositoryNamespace = new Label(groupNamespaceConf, SWT.NONE);
		lblRepositoryNamespace.setText("Repository package:");

		txtRepositoryNamespace = new Text(groupNamespaceConf, SWT.BORDER);
		txtRepositoryNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtRepositoryNamespace.setText(store.getString(PREF_REPOSITORY_CONTEXT));
		txtRepositoryNamespace.addModifyListener(e -> validateRepositoryNamespaceName(txtRepositoryNamespace.getText()));

		repositoryNamespaceName = txtRepositoryNamespace.getText();

		final var lblSubNamespaces = new Label(groupNamespaceConf, SWT.NONE);
		lblSubNamespaces.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));
		lblSubNamespaces.setText("Sub-packages:");

		final var gdListSubNs = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdListSubNs.heightHint = 80;

		listSubNamespaces = new List(groupNamespaceConf, SWT.V_SCROLL | SWT.BORDER | SWT.H_SCROLL);
		listSubNamespaces.setLayoutData(gdListSubNs);

		listSubNamespaces.addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent e) {
				if (e.character == '-') {
					final String[] selItems = listSubNamespaces.getSelection();

					for (final String item : selItems)
						listSubNamespaces.remove(item);
				}
				else if (e.character == '+')
					addSubPackage();
			}
		});

		final var menu = new Menu(listSubNamespaces);
		listSubNamespaces.setMenu(menu);

		// Add the default sub-package
		listSubNamespaces.add(DEFAULT_SUB_PACKAGE);

		final var mnuAdd = new MenuItem(menu, SWT.NONE);
		mnuAdd.setText("Add package");

		mnuAdd.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				addSubPackage();
			}
		});

		final var mnuRemove = new MenuItem(menu, SWT.NONE);
		mnuRemove.setText("Remove package");

		mnuRemove.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final String[] selItems = listSubNamespaces.getSelection();

				for (final String item : selItems)
					listSubNamespaces.remove(item);
			}
		});

		validateProject(projectName);
		validateProjectCode(projectCode);
		validateRootNamespaceName(rootNamespaceName);
		validateDomainNamespace(domainNamespaceName);
		validateRepositoryNamespaceName(repositoryNamespaceName);
		validateDTONamespaceName(dtoNamespaceName);
	}

	/**
	 * Add a new sub-package to the initial project configuration
	 */
	private void addSubPackage() {
		final var msgTitle = "Add new sub-package";

		final var dlg = new InputDialog(getShell(), msgTitle, "Enter the name of the sub-package:", "", newText -> {
			final IStatus status = EclipseIDEService.validatePackageName(newText);

			if (status.getSeverity() > IStatus.INFO)
				return status.getMessage();

			return null;
		});

		if (dlg.open() != Dialog.OK)
			return;

		for (final String item : listSubNamespaces.getItems())
			if (item.equals(dlg.getValue())) {
				MessageDialog.openInformation(getShell(), msgTitle, "A sub-package with the name '" + item + "' already exists!");
				return;
			}

		listSubNamespaces.add(dlg.getValue());
	}

	/**
	 * Validate the project name
	 * @param text
	 */
	private void validateProject(String text) {
		final IWorkspace workspace = ResourcesPlugin.getWorkspace();
		final IStatus status = workspace.validateName(text, IResource.PROJECT);

		updateStatus(status);

		projectName = text;
	}

	/**
	 * Validate the project code
	 * @param text
	 */
	private void validateProjectCode(String text) {
		IStatus status = createStatus(IStatus.INFO, "");

		// This field is used as a prefix of the final artifact name(s). Thus, it must follow common naming conventions!
		if (text == null || text.isEmpty() || !ARTIFACT_NAME_PATTERN.matcher(text).matches())
			status = createStatus(IStatus.ERROR, "The artifact name is not valid!");

		updateStatus(status);

		projectCode = text;
	}

	/**
	 * Validate the domain namespace name
	 * @param text
	 */
	private void validateDomainNamespace(String text) {
		final IStatus status = EclipseIDEService.validatePackageName(text);
		updateStatus(status);

		domainNamespaceName = text;
	}

	/**
	 * Validate the persistence unit name
	 * @param text
	 */
	private void validateRootNamespaceName(String text) {
		final IStatus status = EclipseIDEService.validatePackageName(text);
		updateStatus(status);

		rootNamespaceName = text;
	}

	/**
	 * Validate the repository namespace name
	 * @param text
	 */
	private void validateRepositoryNamespaceName(String text) {
		final IStatus status = EclipseIDEService.validatePackageName(text);
		updateStatus(status);

		repositoryNamespaceName = text;
	}

	/**
	 * Validate the DTO namespace name
	 * @param text
	 */
	private void validateDTONamespaceName(String text) {
		final IStatus status = EclipseIDEService.validatePackageName(text);
		updateStatus(status);

		dtoNamespaceName = text;
	}

	/**
	 * Validate the exchange namespace name
	 * @param text
	 */
	private void validateExchangeNamespaceName(String text) {
		final IStatus status = EclipseIDEService.validatePackageName(text);
		updateStatus(status);

		exchangeNamespaceName = text;
	}

	/**
	 * Validate the boundary namespace name
	 * @param text
	 */
	private void validateBoundaryNamespaceName(String text) {
		final IStatus status = EclipseIDEService.validatePackageName(text);
		updateStatus(status);

		boundaryNamespaceName = text;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.DialogPage#setVisible(boolean)
	 */
	@Override
	public void setVisible(boolean visible) {
		super.setVisible(visible);

		pageVisible = visible;

		if (visible && currentStatus.matches(IStatus.ERROR))
			currentStatus = createStatus(IStatus.ERROR, "");

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
	 * Apply the status to the given dialog page
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
	 * Create a new status
	 * @param severity
	 * @param message
	 * @return the created status
	 */
	private static IStatus createStatus(int severity, String message) {
		return new Status(severity, CodeCadenzaUserInterfacePlugin.PLUGIN_ID, severity, message, null);
	}

	/**
	 * @return the name entered by the user
	 */
	@Override
	public String getName() {
		return projectName;
	}

	/**
	 * @return the root namespace name
	 */
	public String getRootNamespaceName() {
		return rootNamespaceName;
	}

	/**
	 * @return the name of the domain namespace
	 */
	public String getDomainNamespaceName() {
		return domainNamespaceName;
	}

	/**
	 * @return the repository namespace name
	 */
	public String getRepositoryNamespaceName() {
		return repositoryNamespaceName;
	}

	/**
	 * @return true if the boundary mode is selected
	 */
	public boolean isBoundaryMode() {
		return boundaryMode;
	}

	/**
	 * @return the data transfer object namespace name
	 */
	public String getDTONamespaceName() {
		return dtoNamespaceName;
	}

	/**
	 * @return the exchange namespace name
	 */
	public String getExchangeNamespaceName() {
		return exchangeNamespaceName;
	}

	/**
	 * @return the name of the boundary namespace
	 */
	public String getBoundaryNamespaceName() {
		return boundaryNamespaceName;
	}

	/**
	 * @return all sub-namespace names
	 */
	public String[] getSubNamespaceNames() {
		return listSubNamespaces.getItems();
	}

	/**
	 * @return the project code
	 */
	public String getProjectCode() {
		return projectCode;
	}

	/**
	 * @return true if manual changes in the source code should be protected
	 */
	public boolean isProtectManualChanges() {
		return protectManualChanges;
	}

}
