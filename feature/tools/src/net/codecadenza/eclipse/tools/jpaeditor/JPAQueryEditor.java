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
package net.codecadenza.eclipse.tools.jpaeditor;

import static net.codecadenza.eclipse.shared.Constants.IMG_ATTRIBUTE;
import static net.codecadenza.eclipse.shared.Constants.MODEL_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.MODEL_ROOT_FILE;
import static net.codecadenza.eclipse.shared.Constants.PREF_DATE_FORMAT;
import static net.codecadenza.eclipse.shared.Constants.PREF_NUMBER_FORMAT;
import static net.codecadenza.eclipse.shared.Constants.PREF_TIME_FORMAT;

import jakarta.persistence.EntityManager;
import java.io.File;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.UUID;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.tools.CodeCadenzaToolsPlugin;
import net.codecadenza.eclipse.tools.jpaeditor.actions.JPAEditorToolBar;
import net.codecadenza.eclipse.tools.jpaeditor.service.JPAQueryResultSet;
import net.codecadenza.eclipse.tools.jpaeditor.service.JPAQueryService;
import net.codecadenza.eclipse.tools.jpaeditor.util.PersistenceEngine;
import net.codecadenza.eclipse.tools.jpaeditor.viewer.JPASyntax;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.TreeEvent;
import org.eclipse.swt.events.TreeListener;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.EditorPart;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.statushandlers.StatusManager;
import org.hibernate.collection.spi.PersistentBag;

/**
 * <p>
 * JPA query editor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JPAQueryEditor extends EditorPart {
	public static final String ID = "net.codecadenza.eclipse.tools.jpaeditor.JPAQueryEditor";
	private static final String METHOD_NAME_GET_CLASS = "getClass";
	private static final String HIBERNATE_LAZY_INIT = "HIBERNATELAZYINITIALIZE";

	private DateFormat dateTimeFormatter;
	private DecimalFormat decimalFormatter;
	private JPAQueryResultSet rs;
	private PersistenceEngine engine;
	private JPATextEditor textEditor;
	private JPAEditorToolBar toolBar;
	private boolean isDirty;
	private Shell shell;
	private Project project;
	private JPASyntax syntax;
	private String currentSelectedCellValue = "";
	private Label lblStatusMessage;
	private Label lblStatusImage;
	private IWorkbenchWindow workbenchWindow;
	private Tree tree;
	private EntityManager em;

	/**
	 * Execute the given JPA query
	 * @param monitor
	 * @param jpaStatement the JPA query command to be executed
	 * @param limitRows
	 */
	private void doQuery(IProgressMonitor monitor, final String jpaStatement, final int limitRows) {
		final double startTime = System.currentTimeMillis();

		// This method is executed in a separate thread. Thus, we must access the user-interface controls in a proper manner!
		shell.getDisplay().syncExec(() -> {
			tree.setVisible(false);

			// Clear the table
			tree.removeAll();

			// Remove all existing columns
			for (final TreeColumn col : tree.getColumns())
				col.dispose();
		});

		// Check if the user has canceled this operation
		if (monitor.isCanceled()) {
			// Show the tree view
			shell.getDisplay().syncExec(() -> {
				lblStatusMessage.setText("Query canceled by user!");
				lblStatusImage.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_INFO_TSK));
				tree.setVisible(true);
			});

			monitor.done();
			return;
		}

		monitor.worked(1);

		try {
			monitor.subTask("Get data");

			rs = JPAQueryService.executeQuery(jpaStatement, em, limitRows);
			monitor.worked(1);

			if (monitor.isCanceled()) {
				// Show the tree view
				shell.getDisplay().syncExec(() -> {
					lblStatusMessage.setText("Query canceled by user!");
					lblStatusImage.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_INFO_TSK));
					tree.setVisible(true);
				});

				monitor.done();
				return;
			}

			monitor.subTask("Set input");

			shell.getDisplay().syncExec(() -> {
				// Measure the execution time
				final double endTime = System.currentTimeMillis();
				final double timeDif = endTime - startTime;

				var label = " row ";
				var labelPlural = " rows ";

				if (!rs.isFlat()) {
					label = " object ";
					labelPlural = " objects ";
				}

				if (timeDif > 1000) {
					final double timeResult = timeDif / 1000;

					if (rs.getRows().size() == 1)
						lblStatusMessage.setText(
								"Query returned " + rs.getRows().size() + label + "in " + decimalFormatter.format(timeResult) + " seconds.");
					else
						lblStatusMessage.setText("Query returned " + rs.getRows().size() + labelPlural + "in "
								+ decimalFormatter.format(timeResult) + " seconds.");
				}
				else if (rs.getRows().size() == 1)
					lblStatusMessage.setText(
							"Query returned " + rs.getRows().size() + label + "in " + decimalFormatter.format(timeDif) + " milliseconds.");
				else
					lblStatusMessage.setText("Query returned " + rs.getRows().size() + labelPlural + "in "
							+ decimalFormatter.format(timeDif) + " milliseconds.");

				lblStatusImage.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_INFO_TSK));

				tree.setLinesVisible(rs.isFlat());
				tree.setHeaderVisible(rs.isFlat());

				// Change the input
				buildTree();
			});

			monitor.worked(1);
		}
		catch (final Exception e) {
			CodeCadenzaToolsPlugin.getInstance().logError(e);

			shell.getDisplay().syncExec(() -> {
				lblStatusMessage.setText("Error while executing query! Message: " + e.getMessage());
				lblStatusImage.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_ERROR_TSK));
			});
		}
		finally {
			shell.getDisplay().syncExec(() -> tree.setVisible(true));

			monitor.done();
		}
	}

	/**
	 * Add the items of a persistent bag (e.g. Collection) to the given parent tree item
	 * @param parentItem
	 * @return true if the {@link PersistentBag} contained entities that were added to the tree. It will return false if the objects
	 *         were not added to the tree, since the {@link PersistentBag} was an element collection.
	 */
	protected boolean addPersistentBagItems(TreeItem parentItem) {
		parentItem.removeAll();

		final var persistentBag = (PersistentBag<?>) parentItem.getData();
		int count = 1;

		for (final Object persistentObject : persistentBag.toArray()) {
			final Class<?> objectClass = persistentObject.getClass();

			if (isSupportedFieldType(objectClass))
				return false;

			final String className = objectClass.getName().substring(objectClass.getName().lastIndexOf('.') + 1);

			final var arrayTreeItem = new TreeItem(parentItem, SWT.NONE);
			arrayTreeItem.setText(className + " [" + count++ + "]");
			arrayTreeItem.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PUBLIC));
			arrayTreeItem.setData(persistentObject);

			new TreeItem(arrayTreeItem, SWT.NONE);
		}

		return true;
	}

	/**
	 * Add the associations to the tree item
	 * @param parentItem the treeItem to open
	 */
	private void addAssociations(TreeItem parentItem) {
		final Object object = parentItem.getData();

		try {
			for (final Method method : object.getClass().getMethods()) {
				if (method.getName().startsWith("get") && method.getParameterTypes().length == 0
						&& !METHOD_NAME_GET_CLASS.equals(method.getName()) && !method.getName().toUpperCase().contains(HIBERNATE_LAZY_INIT)) {
					final Object ret = method.invoke(object);

					if (!isSupportedFieldType(method.getReturnType()) && !(ret instanceof Enum)) {
						String assocName = method.getName().substring(3);
						assocName = assocName.substring(0, 1).toLowerCase() + assocName.substring(1);
						final String retTypeName = method.getReturnType().getName()
								.substring(method.getReturnType().getName().lastIndexOf('.') + 1);

						final var treeItem = new TreeItem(parentItem, SWT.NONE);
						treeItem.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PUBLIC));

						if (ret != null) {
							treeItem.setData(ret);
							treeItem.setText(retTypeName + " " + assocName);
							new TreeItem(treeItem, SWT.NONE);
						}
						else
							treeItem.setText(retTypeName + " " + assocName + " = " + JPAQueryService.NULL_VALUE);
					}
				}
			}
		}
		catch (final Exception e) {
			final var s = new Status(IStatus.WARNING, CodeCadenzaToolsPlugin.PLUGIN_ID, 0, "Error while fetching data!", e);
			StatusManager.getManager().handle(s, StatusManager.LOG | StatusManager.SHOW);
		}
	}

	/**
	 * Check if the given class is a supported type for persistent attributes
	 * @param objectClass
	 * @return true if the class is supported for persistent attributes
	 */
	private boolean isSupportedFieldType(Class<?> objectClass) {
		return (objectClass == Integer.class || objectClass == int.class || objectClass == Long.class || objectClass == long.class
				|| objectClass == Double.class || objectClass == double.class || objectClass == Float.class || objectClass == float.class
				|| objectClass == Boolean.class || objectClass == boolean.class || objectClass == String.class
				|| objectClass == char.class || objectClass == BigDecimal.class || objectClass == Date.class
				|| objectClass == LocalDateTime.class || objectClass == LocalDate.class || objectClass == GregorianCalendar.class
				|| objectClass == UUID.class);
	}

	/**
	 * Build the item text
	 * @param returnType
	 * @param getterName
	 * @param value
	 * @return the generated text
	 */
	private String createItemText(Class<?> returnType, String getterName, String value) {
		final var b = new StringBuilder();
		String attributeName;
		final String retTypeName = returnType.getName().substring(returnType.getName().lastIndexOf('.') + 1);

		if (getterName.startsWith("get")) {
			final String getter = getterName.substring(3);
			attributeName = getter.substring(0, 1).toLowerCase() + getter.substring(1);
		}
		else {
			final String getter = getterName.substring(2);
			attributeName = getter.substring(0, 1).toLowerCase() + getter.substring(1);
		}

		b.append(attributeName);
		b.append(" : ");
		b.append(retTypeName);
		b.append(" = ");

		if (value != null) {
			if (!value.isEmpty()) {
				// Return only the first line!
				if (value.contains("\n"))
					value = value.substring(0, value.indexOf('\n')) + "...";
				else if (value.contains("\r"))
					value = value.substring(0, value.indexOf('\r')) + "...";

				b.append(value);
			}
			else
				b.append(JPAQueryService.EMPTY_STRING);
		}
		else
			b.append(JPAQueryService.NULL_VALUE);

		return b.toString();
	}

	/**
	 * Add the attributes to the tree item
	 * @param parentItem the treeItem to open
	 * @return false if the items of an element collection has been added
	 */
	private boolean addAttributes(TreeItem parentItem) {
		final Object object = parentItem.getData();

		try {
			if (object instanceof final Iterable<?> iterable) {
				iterable.forEach(item -> {
					final var listItem = new TreeItem(parentItem, SWT.NONE);
					listItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));

					if (item != null)
						listItem.setText(item.toString());
					else
						listItem.setText(JPAQueryService.NULL_VALUE);
				});

				return false;
			}

			for (final Method method : object.getClass().getMethods()) {
				if ((method.getName().startsWith("get") || method.getName().startsWith("is")) && method.getParameterTypes().length == 0
						&& !METHOD_NAME_GET_CLASS.equals(method.getName()) && !method.getName().toUpperCase().contains(HIBERNATE_LAZY_INIT)) {
					final Object returnValue = method.invoke(object);
					String convertedValue = null;
					boolean addItem = false;

					if (method.getReturnType() == Double.class || method.getReturnType() == double.class) {
						final var value = (Double) returnValue;
						convertedValue = value != null ? decimalFormatter.format(value) : null;
						addItem = true;
					}
					else if (method.getReturnType() == Float.class || method.getReturnType() == float.class) {
						final var value = (Float) returnValue;
						convertedValue = value != null ? decimalFormatter.format(value) : null;
						addItem = true;
					}
					else if (method.getReturnType() == BigDecimal.class) {
						final var value = (BigDecimal) returnValue;
						convertedValue = value != null ? decimalFormatter.format(value) : null;
						addItem = true;
					}
					else if (method.getReturnType() == String.class) {
						final var value = (String) returnValue;
						convertedValue = value;
						addItem = true;
					}
					else if (method.getReturnType() == Date.class) {
						final var value = (Date) returnValue;
						convertedValue = value != null ? dateTimeFormatter.format(value) : null;
						addItem = true;
					}
					else if (method.getReturnType() == LocalDateTime.class) {
						final var value = (LocalDateTime) returnValue;
						addItem = true;

						if (value != null) {
							final Date date = Date.from(value.atZone(ZoneId.systemDefault()).toInstant());
							convertedValue = dateTimeFormatter.format(date);
						}
					}
					else if (method.getReturnType() == LocalDate.class) {
						final var value = (LocalDate) returnValue;
						addItem = true;

						if (value != null) {
							final Date date = Date.from(value.atStartOfDay(ZoneOffset.systemDefault()).toInstant());
							convertedValue = dateTimeFormatter.format(date);
						}
					}
					else if (method.getReturnType() == GregorianCalendar.class) {
						final var value = (GregorianCalendar) returnValue;
						addItem = true;
						convertedValue = value != null ? dateTimeFormatter.format(value.getTime()) : null;
					}
					else if (returnValue instanceof Enum) {
						final var value = (Enum<?>) returnValue;
						addItem = true;

						if (value != null)
							convertedValue = value.name();
					}
					else if (method.getReturnType() == Integer.class || method.getReturnType() == int.class
							|| method.getReturnType() == Long.class || method.getReturnType() == long.class
							|| method.getReturnType() == UUID.class) {
						addItem = true;
						convertedValue = returnValue != null ? returnValue.toString() : null;
					}

					if (addItem)
						addAttributeItem(parentItem, method.getReturnType(), method.getName(), convertedValue);
				}
			}
		}
		catch (final Exception e) {
			final var s = new Status(IStatus.WARNING, CodeCadenzaToolsPlugin.PLUGIN_ID, 0, "Error while fetching data!", e);
			StatusManager.getManager().handle(s, StatusManager.LOG | StatusManager.SHOW);
		}

		return true;
	}

	/**
	 * Add the association child items to the tree item
	 * @param parentItem the parent item
	 */
	private void addAssociationChildItems(TreeItem parentItem) {
		parentItem.removeAll();

		final boolean addAssociations = addAttributes(parentItem);

		if (addAssociations)
			addAssociations(parentItem);
	}

	/**
	 * Add an item that represents a persistent attribute to the given parent item
	 * @param parentItem the parent item
	 * @param returnType the return type
	 * @param methodName the method name
	 * @param value the attribute value
	 */
	private void addAttributeItem(TreeItem parentItem, Class<?> returnType, String methodName, String value) {
		final var attributeItem = new TreeItem(parentItem, SWT.NONE);
		attributeItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
		attributeItem.setText(createItemText(returnType, methodName, value));
	}

	/**
	 * Build the tree
	 */
	protected void buildTree() {
		// Add the columns
		boolean firstColumn = true;

		for (final String colName : rs.getHeader()) {
			final var column = new TreeColumn(tree, SWT.NONE);

			if (firstColumn) {
				column.setWidth(350);
				firstColumn = false;
			}
			else
				column.setWidth(150);

			column.setText(colName.toUpperCase());
		}

		int position = 0;
		int counter = 1;

		for (final List<String> row : rs.getRows()) {
			final var item = new TreeItem(tree, SWT.NONE);

			for (int i = 0; i < tree.getColumnCount(); i++) {
				if (rs.isFlat())
					item.setText(i, row.get(i));
				else
					item.setText(i, row.get(i) + " [" + counter++ + "]");
			}

			if (!rs.getRowObjects().isEmpty()) {
				item.setData(rs.getRowObjects().get(position));
				item.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PUBLIC));
				new TreeItem(item, SWT.NONE);
			}

			position++;
		}
	}

	/**
	 * Eliminate all comments from the query
	 * @param jpa
	 * @return the query statement without comments
	 */
	private String eliminateComments(String jpa) {
		var newCommand = "";

		boolean isFirstLine = true;

		// Remove all single-line comments
		final String[] commandLines = jpa.trim().split(System.lineSeparator());

		for (String line : commandLines) {
			line = line.trim();

			if (!line.startsWith("//") && !line.isEmpty()) {
				if (isFirstLine)
					isFirstLine = false;
				else
					newCommand += " ";

				newCommand += line;
			}
		}

		// Remove all multi-line comments
		while (newCommand.contains("/*") && newCommand.contains("*/")) {
			if (newCommand.indexOf("*/") > newCommand.indexOf("/*")) {
				final String multiComment = newCommand.substring(newCommand.indexOf("/*"), newCommand.indexOf("*/") + 2);
				newCommand = newCommand.replace(multiComment, "").trim();
			}
		}

		return newCommand;
	}

	/**
	 * Execute a JPA query
	 */
	public void executeQuery() {
		final String jpaStatement = eliminateComments(textEditor.getJPATextViewer().getTextWidget().getText());

		// Get the preference store
		final IPreferenceStore store = CodeCadenzaToolsPlugin.getInstance().getPreferenceStore();

		// Initialize formatting rules
		dateTimeFormatter = new SimpleDateFormat(store.getString(PREF_DATE_FORMAT) + " " + store.getString(PREF_TIME_FORMAT));
		decimalFormatter = new DecimalFormat(store.getString(PREF_NUMBER_FORMAT));
		final int limitRows = toolBar.getLimitResults();

		if (engine == null) {
			final Cursor defaultCursor = shell.getCursor();

			try {
				shell.setCursor(Display.getCurrent().getSystemCursor(SWT.CURSOR_WAIT));

				// Initialize the persistence engine and the entity manager
				engine = PersistenceEngine.initialize(project);
				em = engine.getEntityManager();

				shell.setCursor(defaultCursor);
			}
			catch (final Exception e) {
				shell.setCursor(defaultCursor);
				CodeCadenzaToolsPlugin.getInstance().handleInternalError(e);
				return;
			}
		}

		try {
			if (em.getTransaction().isActive())
				em.getTransaction().rollback();

			em.getTransaction().begin();

			workbenchWindow.run(true, true, monitor -> {
				// Begin a new task including three steps
				monitor.beginTask("Perform query...", 3);
				doQuery(monitor, jpaStatement, limitRows);
			});
		}
		catch (final InterruptedException ex) {
			Thread.currentThread().interrupt();

			shell.getDisplay().syncExec(() -> {
				lblStatusMessage.setText("Query canceled by user!");
				lblStatusImage.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_INFO_TSK));
			});
		}
		catch (final Exception ex) {
			CodeCadenzaToolsPlugin.getInstance().logError(ex);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.EditorPart#init(org.eclipse.ui.IEditorSite, org.eclipse.ui.IEditorInput)
	 */
	@Override
	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		// Configure the editor
		setSite(site);
		setInput(input);

		final var fileInput = (FileEditorInput) input;

		// Get the project where the file is located
		final IProject proj = fileInput.getFile().getParent().getProject();

		// Get the model file
		final var f = new File(
				proj.getLocation().toString() + File.separatorChar + MODEL_FOLDER + File.separatorChar + MODEL_ROOT_FILE);

		if (f.exists()) {
			// Load the project from the respective file
			final var resourceSet = new ResourceSetImpl();

			final URI namespaceURI = URI.createFileURI(f.getAbsolutePath());

			// Get the resource object
			final Resource projectResource = resourceSet.getResource(namespaceURI, true);

			for (final EObject eObject : projectResource.getContents()) {
				if (eObject instanceof final Project aProject) {
					project = aProject;
					break;
				}
			}
		}

		if (project == null)
			throw new PartInitException(
					"The editor could not be initialized! The selected file doesn't belong to a CodeCadenza project!");

		// Initialize the JPA syntax object
		syntax = new JPASyntax(project);

		// Create the text editor
		textEditor = new JPATextEditor(this, syntax);

		shell = this.getSite().getShell();
		workbenchWindow = this.getEditorSite().getWorkbenchWindow();

		textEditor.init(site, input);

		updatePartName();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createPartControl(Composite parent) {
		final var panEditorArea = new Composite(parent, SWT.NONE);
		panEditorArea.setLayout(new GridLayout());

		toolBar = new JPAEditorToolBar(panEditorArea, this);

		final var sashForm = new SashForm(panEditorArea, SWT.NONE);
		sashForm.setOrientation(SWT.VERTICAL);
		sashForm.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		textEditor.createPartControl(sashForm);

		tree = new Tree(sashForm, SWT.NONE);
		tree.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		tree.setHeaderVisible(true);
		tree.setLinesVisible(false);

		tree.addTreeListener(new TreeListener() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.TreeListener#treeExpanded(org.eclipse.swt.events.TreeEvent)
			 */
			@Override
			public void treeExpanded(TreeEvent e) {
				final var item = (TreeItem) e.item;

				if (item.getData() == null)
					return;

				if (item.getData().getClass() == PersistentBag.class && addPersistentBagItems(item))
					return;

				addAssociationChildItems(item);
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.TreeListener#treeCollapsed(org.eclipse.swt.events.TreeEvent)
			 */
			@Override
			public void treeCollapsed(TreeEvent e) {
				// No implementation required!
			}
		});

		// Create the context menu for the table
		final var mnuTree = new Menu(tree);
		tree.setMenu(mnuTree);

		final var mnuItemCopy = new MenuItem(mnuTree, SWT.CASCADE);
		mnuItemCopy.setText("Copy");

		final var menuCopy = new Menu(mnuItemCopy);
		mnuItemCopy.setMenu(menuCopy);

		final var mnuCopyCellToClipboard = new MenuItem(menuCopy, SWT.NONE);
		mnuCopyCellToClipboard.setText("Copy cell value");

		mnuCopyCellToClipboard.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final var clipboard = new Clipboard(shell.getDisplay());
				clipboard.clearContents();
				clipboard.setContents(new Object[] { currentSelectedCellValue }, new Transfer[] { TextTransfer.getInstance() });
				clipboard.dispose();
			}
		});

		final var mnuCopySelRowsToClipboard = new MenuItem(menuCopy, SWT.NONE);
		mnuCopySelRowsToClipboard.setText("Copy selected rows");

		mnuCopySelRowsToClipboard.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem[] selItems = tree.getSelection();

				if (selItems == null || selItems.length < 1)
					return;

				final var b = new StringBuilder();

				for (final TreeItem item : selItems) {
					for (int i = 0; i < tree.getColumnCount(); i++) {
						if (i != 0)
							b.append(SWT.TAB);

						b.append(item.getText(i));
					}

					b.append(System.lineSeparator());
				}

				final var clipboard = new Clipboard(shell.getDisplay());
				clipboard.clearContents();
				clipboard.setContents(new Object[] { b.toString() }, new Transfer[] { TextTransfer.getInstance() });
				clipboard.dispose();
			}
		});

		final var mnuCopyAllRowsToClipboard = new MenuItem(menuCopy, SWT.NONE);
		mnuCopyAllRowsToClipboard.setText("Copy all rows");

		mnuCopyAllRowsToClipboard.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem[] items = tree.getItems();

				if (items == null || items.length < 1)
					return;

				final var b = new StringBuilder();

				for (final TreeItem item : items) {
					for (int i = 0; i < tree.getColumnCount(); i++) {
						if (i != 0)
							b.append(SWT.TAB);

						b.append(item.getText(i));
					}

					b.append(System.lineSeparator());
				}

				final var clipboard = new Clipboard(shell.getDisplay());
				clipboard.clearContents();
				clipboard.setContents(new Object[] { b.toString() }, new Transfer[] { TextTransfer.getInstance() });
				clipboard.dispose();
			}
		});

		// Track the selected cell value
		tree.addListener(SWT.MouseDown, event -> {
			final var pt = new Point(event.x, event.y);

			for (final TreeItem item : tree.getItems())
				for (int i = 0; i < tree.getColumnCount(); i++) {
					final Rectangle rect = item.getBounds(i);

					if (rect.contains(pt)) {
						currentSelectedCellValue = item.getText(i);
						return;
					}
				}
		});

		// Copy the cell value to the system clipboard if the user presses 'CTRL+C'
		tree.addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent e) {
				if (e.stateMask == SWT.CTRL && e.keyCode != SWT.CTRL && e.keyCode == 99) {
					final var clipboard = new Clipboard(shell.getDisplay());
					clipboard.clearContents();
					clipboard.setContents(new Object[] { currentSelectedCellValue }, new Transfer[] { TextTransfer.getInstance() });
					clipboard.dispose();
				}
			}
		});

		final var glStatus = new GridLayout(2, false);
		glStatus.marginHeight = 2;
		glStatus.marginWidth = 0;

		final var panStatus = new Composite(panEditorArea, SWT.BORDER);
		panStatus.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		panStatus.setLayout(glStatus);

		lblStatusImage = new Label(panStatus, SWT.NONE);
		lblStatusImage.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_INFO_TSK));

		lblStatusMessage = new Label(panStatus, SWT.NONE);
		lblStatusMessage.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		lblStatusMessage.setText("No query performed!");

		sashForm.setWeights(20, 80);

		// Refresh the text viewer in order to initialize syntax highlighting
		textEditor.getJPATextViewer().refresh(syntax);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.EditorPart#setInput(org.eclipse.ui.IEditorInput)
	 */
	@Override
	protected void setInput(IEditorInput input) {
		super.setInput(input);

		if (textEditor != null)
			textEditor.setInput(input);

		updatePartName();
	}

	/**
	 * Update the editor part name
	 */
	private void updatePartName() {
		Display.getDefault().syncExec(() -> {
			final String name = getEditorInput().getName();

			setPartName(name);
		});
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#dispose()
	 */
	@Override
	public void dispose() {
		super.dispose();

		try {
			if (em != null) {
				if (em.getTransaction().isActive())
					em.getTransaction().rollback();

				em.close();
			}

			if (engine != null)
				engine.close();
		}
		catch (final Exception e) {
			// This exception will be ignored!
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.EditorPart#doSave(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public void doSave(IProgressMonitor monitor) {
		save(monitor);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.EditorPart#doSaveAs()
	 */
	@Override
	public void doSaveAs() {
		save(null);
	}

	/**
	 * @param monitor
	 * @return always true
	 */
	private boolean save(IProgressMonitor monitor) {
		if (monitor == null)
			monitor = textEditor.getProgressMonitor();

		textEditor.doSave(monitor);
		setIsDirty(textEditor.isDirty());

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.EditorPart#isDirty()
	 */
	@Override
	public boolean isDirty() {
		return isDirty || textEditor.isDirty();
	}

	/**
	 * @param isDirty
	 */
	protected void setIsDirty(boolean isDirty) {
		if (this.isDirty != isDirty) {
			this.isDirty = isDirty;
			firePropertyChange(IEditorPart.PROP_DIRTY);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
	 */
	@Override
	public void setFocus() {
		textEditor.setFocus();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.EditorPart#isSaveAsAllowed()
	 */
	@Override
	public boolean isSaveAsAllowed() {
		return textEditor.isSaveAsAllowed();
	}

	/**
	 * Clear the text in the editor
	 */
	public void clearText() {
		textEditor.getJPATextViewer().clearText();
	}

}
