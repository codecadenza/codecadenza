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
package net.codecadenza.eclipse.diagram.domain.parsers;

import java.util.Arrays;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.diagram.domain.part.Messages;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EEnumLiteral;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.emf.transaction.util.TransactionUtil;
import org.eclipse.gmf.runtime.common.core.command.ICommand;
import org.eclipse.gmf.runtime.common.core.command.UnexecutableCommand;
import org.eclipse.gmf.runtime.common.ui.services.parser.IParser;
import org.eclipse.gmf.runtime.common.ui.services.parser.IParserEditStatus;
import org.eclipse.gmf.runtime.common.ui.services.parser.ParserEditStatus;
import org.eclipse.gmf.runtime.emf.commands.core.command.CompositeTransactionalCommand;
import org.eclipse.gmf.runtime.emf.type.core.commands.SetValueCommand;
import org.eclipse.gmf.runtime.emf.type.core.requests.SetRequest;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.osgi.util.NLS;

/**
 * <p>
 * Abstract parser
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractParser implements IParser {
	protected final EAttribute[] features;
	private String viewPattern;
	private String editorPattern;
	private String editPattern;

	/**
	 * @param features
	 */
	protected AbstractParser(EAttribute[] features) {
		if (features == null || Arrays.asList(features).contains(null))
			throw new IllegalArgumentException();

		this.features = features;
	}

	/**
	 * @return the view pattern
	 */
	public String getViewPattern() {
		return viewPattern;
	}

	/**
	 * @param viewPattern
	 */
	public void setViewPattern(String viewPattern) {
		this.viewPattern = viewPattern;
	}

	/**
	 * @return the editor pattern
	 */
	public String getEditorPattern() {
		return editorPattern;
	}

	/**
	 * @param editorPattern
	 */
	public void setEditorPattern(String editorPattern) {
		this.editorPattern = editorPattern;
	}

	/**
	 * @return edit pattern
	 */
	public String getEditPattern() {
		return editPattern;
	}

	/**
	 * @param editPattern
	 */
	public void setEditPattern(String editPattern) {
		this.editPattern = editPattern;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.common.ui.services.parser.IParser#isAffectingEvent(java.lang.Object, int)
	 */
	@Override
	public boolean isAffectingEvent(Object event, int flags) {
		if (event instanceof final Notification notification)
			return isAffectingFeature(notification.getFeature());

		return false;
	}

	/**
	 * @param feature
	 * @return true if the given feature is affected
	 */
	protected boolean isAffectingFeature(Object feature) {
		return Arrays.asList(features).stream().anyMatch(aFeature -> aFeature == feature);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.common.ui.services.parser.IParser#getCompletionProcessor(org.eclipse.core.runtime.IAdaptable)
	 */
	@Override
	public IContentAssistProcessor getCompletionProcessor(IAdaptable element) {
		return null;
	}

	/**
	 * @param element
	 * @return an array of values
	 */
	protected Object[] getValues(EObject element) {
		final var values = new Object[features.length];

		for (int i = 0; i < features.length; i++)
			values[i] = getValue(element, features[i]);

		return values;
	}

	/**
	 * @param element
	 * @param feature
	 * @return the value
	 */
	protected Object getValue(EObject element, EAttribute feature) {
		Object value = element.eGet(feature);
		final Class<?> iClass = feature.getEAttributeType().getInstanceClass();

		if (String.class.equals(iClass) && value == null)
			value = "";

		return value;
	}

	/**
	 * @param adapter
	 * @param values
	 * @param flags
	 * @return the command
	 */
	@SuppressWarnings("unused")
	protected ICommand getParseCommand(IAdaptable adapter, Object[] values, int flags) {
		if (values == null || validateNewValues(values).getCode() != IParserEditStatus.EDITABLE)
			return UnexecutableCommand.INSTANCE;

		final EObject element = adapter.getAdapter(EObject.class);
		final TransactionalEditingDomain editingDomain = TransactionUtil.getEditingDomain(element);

		if (editingDomain == null)
			return UnexecutableCommand.INSTANCE;

		final var command = new CompositeTransactionalCommand(editingDomain, "Set Values");

		for (int i = 0; i < values.length; i++)
			command.compose(getModificationCommand(element, features[i], values[i]));

		return command;
	}

	/**
	 * @param element
	 * @param feature
	 * @param value
	 * @return the modification command
	 */
	protected ICommand getModificationCommand(EObject element, EAttribute feature, Object value) {
		value = getValidNewValue(feature, value);

		if (value instanceof InvalidValue)
			return UnexecutableCommand.INSTANCE;

		final var request = new SetRequest(element, feature, value);

		return new SetValueCommand(request);
	}

	/**
	 * @param values
	 * @return the parser status
	 */
	protected IParserEditStatus validateNewValues(Object[] values) {
		if (values.length != features.length)
			return ParserEditStatus.UNEDITABLE_STATUS;

		for (int i = 0; i < values.length; i++) {
			final Object value = getValidNewValue(features[i], values[i]);

			if (value instanceof InvalidValue)
				return new ParserEditStatus(CodeCadenzaDiagramEditorPlugin.ID, IParserEditStatus.UNEDITABLE, value.toString());
		}

		return ParserEditStatus.EDITABLE_STATUS;
	}

	/**
	 * @param feature
	 * @param value
	 * @return the new value
	 */
	protected Object getValidNewValue(EAttribute feature, Object value) {
		final EClassifier type = feature.getEType();

		if (type instanceof EDataType) {
			final Class<?> iClass = type.getInstanceClass();

			if (Boolean.TYPE.equals(iClass)) {
				if (value instanceof Boolean) {
					// No conversion necessary!
				}
				else if (value instanceof final String stringValue)
					value = Boolean.valueOf(stringValue);
				else
					value = new InvalidValue(NLS.bind(Messages.AbstractParser_UnexpectedValueTypeMessage, iClass.getName()));
			}
			else if (Character.TYPE.equals(iClass)) {
				if (value instanceof Character) {
					// No conversion necessary!
				}
				else if (value instanceof final String stringValue) {
					if (stringValue.isEmpty())
						value = null;
					else
						value = Character.valueOf(stringValue.charAt(0));
				}
				else
					value = new InvalidValue(NLS.bind(Messages.AbstractParser_UnexpectedValueTypeMessage, iClass.getName()));
			}
			else if (Byte.TYPE.equals(iClass)) {
				if (value instanceof Byte) {
					// No conversion necessary!
				}
				else if (value instanceof final Number number)
					value = Byte.valueOf(number.byteValue());
				else if (value instanceof final String stringValue) {
					if (stringValue.isEmpty())
						value = null;
					else {
						try {
							value = Byte.valueOf(stringValue);
						}
						catch (final NumberFormatException _) {
							value = new InvalidValue(NLS.bind(Messages.AbstractParser_WrongStringConversionMessage, iClass.getName()));
						}
					}
				}
				else
					value = new InvalidValue(NLS.bind(Messages.AbstractParser_UnexpectedValueTypeMessage, iClass.getName()));
			}
			else if (Short.TYPE.equals(iClass)) {
				if (value instanceof Short) {
					// No conversion necessary!
				}
				else if (value instanceof final Number number)
					value = Short.valueOf(number.shortValue());
				else if (value instanceof final String stringValue) {
					if (stringValue.isEmpty())
						value = null;
					else {
						try {
							value = Short.valueOf(stringValue);
						}
						catch (final NumberFormatException _) {
							value = new InvalidValue(NLS.bind(Messages.AbstractParser_WrongStringConversionMessage, iClass.getName()));
						}
					}
				}
				else
					value = new InvalidValue(NLS.bind(Messages.AbstractParser_UnexpectedValueTypeMessage, iClass.getName()));
			}
			else if (Integer.TYPE.equals(iClass)) {
				if (value instanceof Integer) {
					// No conversion necessary!
				}
				else if (value instanceof final Number number)
					value = Integer.valueOf(number.intValue());
				else if (value instanceof final String stringValue) {
					if (stringValue.isEmpty())
						value = null;
					else {
						try {
							value = Integer.valueOf(stringValue);
						}
						catch (final NumberFormatException _) {
							value = new InvalidValue(NLS.bind(Messages.AbstractParser_WrongStringConversionMessage, iClass.getName()));
						}
					}
				}
				else
					value = new InvalidValue(NLS.bind(Messages.AbstractParser_UnexpectedValueTypeMessage, iClass.getName()));
			}
			else if (Long.TYPE.equals(iClass)) {
				if (value instanceof Long) {
					// No conversion necessary!
				}
				else if (value instanceof final Number number)
					value = Long.valueOf(number.longValue());
				else if (value instanceof final String stringValue) {
					if (stringValue.isEmpty())
						value = null;
					else {
						try {
							value = Long.valueOf(stringValue);
						}
						catch (final NumberFormatException _) {
							value = new InvalidValue(NLS.bind(Messages.AbstractParser_WrongStringConversionMessage, iClass.getName()));
						}
					}
				}
				else
					value = new InvalidValue(NLS.bind(Messages.AbstractParser_UnexpectedValueTypeMessage, iClass.getName()));
			}
			else if (Float.TYPE.equals(iClass)) {
				if (value instanceof Float) {
					// No conversion necessary!
				}
				else if (value instanceof final Number number)
					value = Float.valueOf(number.floatValue());
				else if (value instanceof final String stringValue) {
					if (stringValue.isEmpty())
						value = null;
					else {
						try {
							value = Float.valueOf(stringValue);
						}
						catch (final NumberFormatException _) {
							value = new InvalidValue(NLS.bind(Messages.AbstractParser_WrongStringConversionMessage, iClass.getName()));
						}
					}
				}
				else
					value = new InvalidValue(NLS.bind(Messages.AbstractParser_UnexpectedValueTypeMessage, iClass.getName()));
			}
			else if (Double.TYPE.equals(iClass)) {
				if (value instanceof Double) {
					// No conversion necessary!
				}
				else if (value instanceof final Number number)
					value = Double.valueOf(number.doubleValue());
				else if (value instanceof final String stringValue) {
					if (stringValue.isEmpty())
						value = null;
					else {
						try {
							value = Double.valueOf(stringValue);
						}
						catch (final NumberFormatException _) {
							value = new InvalidValue(NLS.bind(Messages.AbstractParser_WrongStringConversionMessage, iClass.getName()));
						}
					}
				}
				else
					value = new InvalidValue(NLS.bind(Messages.AbstractParser_UnexpectedValueTypeMessage, iClass.getName()));
			}
			else if (type instanceof final EEnum eenum) {
				if (value instanceof final String stringValue) {
					final EEnumLiteral literal = eenum.getEEnumLiteralByLiteral(stringValue);

					if (literal == null)
						value = new InvalidValue(NLS.bind(Messages.AbstractParser_UnknownLiteralMessage, value));
					else
						value = literal.getInstance();
				}
				else
					value = new InvalidValue(NLS.bind(Messages.AbstractParser_UnexpectedValueTypeMessage, String.class.getName()));
			}
		}

		return value;
	}

	protected static class InvalidValue {
		private final String description;

		/**
		 * @param description
		 */
		public InvalidValue(String description) {
			this.description = description;
		}

		/*
		 * (non-Javadoc)
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			return description;
		}
	}

}
