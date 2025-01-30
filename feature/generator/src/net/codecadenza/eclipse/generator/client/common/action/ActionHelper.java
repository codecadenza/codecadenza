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
package net.codecadenza.eclipse.generator.client.common.action;

import java.util.List;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;

/**
 * <p>
 * Utility class that determines default actions for forms and grid panels
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ActionHelper {
	/**
	 * Prevent instantiation
	 */
	private ActionHelper() {

	}

	/**
	 * @param formPanel
	 * @param securityEnabled
	 * @return the default update action of a form panel. If no corresponding action can be found null will be returned!
	 */
	public static FormAction getDefaultUpdateAction(FormPanel formPanel, boolean securityEnabled) {
		return getDefaultUpdateAction(formPanel.getActions(), securityEnabled);
	}

	/**
	 * @param form
	 * @param securityEnabled
	 * @return the default update action of a form. If no corresponding action can be found null will be returned!
	 */
	public static FormAction getDefaultUpdateAction(Form form, boolean securityEnabled) {
		return getDefaultUpdateAction(form.getActions(), securityEnabled);
	}

	/**
	 * @param formPanel
	 * @param securityEnabled
	 * @return the default read-only action of a form panel. If no corresponding action can be found null will be returned!
	 */
	public static FormAction getDefaultReadOnlyAction(FormPanel formPanel, boolean securityEnabled) {
		FormAction defaultReadOnlyAction = null;

		for (final FormAction action : formPanel.getActions()) {
			if (action.getTargetForm() == null)
				continue;

			final Form targetForm = action.getTargetForm();

			// We just use the first action we can find!
			if (targetForm.getFormType() == FormTypeEnumeration.READONLY) {
				if (securityEnabled && action.getRoles().isEmpty())
					continue;

				defaultReadOnlyAction = action;
			}
		}

		return defaultReadOnlyAction;
	}

	/**
	 * @param form
	 * @param securityEnabled
	 * @return the default read-only action of a form. If no corresponding action can be found null will be returned!
	 */
	public static FormAction getDefaultReadOnlyAction(Form form, boolean securityEnabled) {
		final FormAction defaultUpdateAction = getDefaultUpdateAction(form.getActions(), securityEnabled);
		FormAction defaultReadOnlyAction = null;

		// If security isn't enabled and a default update action exists we won't need a read-only action!
		if (!securityEnabled && defaultUpdateAction != null)
			return null;

		for (final FormAction action : form.getActions()) {
			if (action.getTargetForm() == null)
				continue;

			final Form targetForm = action.getTargetForm();

			// We just use the first action we can find!
			if (targetForm.getFormType() == FormTypeEnumeration.READONLY) {
				if (securityEnabled && action.getRoles().isEmpty())
					continue;

				defaultReadOnlyAction = action;
			}
		}

		if (defaultReadOnlyAction == null)
			return null;

		// If both actions share the same roles it won't make sense to return the read-only action!
		if (securityEnabled && defaultUpdateAction != null
				&& defaultReadOnlyAction.getRoles().containsAll(defaultUpdateAction.getRoles())
				&& defaultUpdateAction.getRoles().containsAll(defaultReadOnlyAction.getRoles()))
			return null;

		return defaultReadOnlyAction;
	}

	/**
	 * @param actions
	 * @param securityEnabled
	 * @return the default update action. If no corresponding action can be found null will be returned!
	 */
	private static FormAction getDefaultUpdateAction(List<FormAction> actions, boolean securityEnabled) {
		FormAction updateAction = null;

		for (final FormAction action : actions) {
			if (action.getTargetForm() == null)
				continue;

			final Form targetForm = action.getTargetForm();

			// We just return the first action we can find!
			if (targetForm.getFormType() == FormTypeEnumeration.UPDATE) {
				if (securityEnabled && action.getRoles().isEmpty())
					continue;

				updateAction = action;
			}
		}

		return updateAction;
	}

}
