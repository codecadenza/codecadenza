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
package net.codecadenza.runtime.webclient.primefaces.util;

import jakarta.faces.application.FacesMessage;
import jakarta.faces.context.FacesContext;
import java.text.MessageFormat;
import java.util.ResourceBundle;
import net.codecadenza.runtime.exception.ExceptionHelper;

/**
 * <p>
 * Utility class to send localized JSF messages
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MessageUtil {
	/**
	 * Prevent instantiation
	 */
	private MessageUtil() {

	}

	/**
	 * @param bundle
	 * @param severity
	 * @param msgKey
	 */
	public static void sendFacesMessage(ResourceBundle bundle, FacesMessage.Severity severity, String msgKey) {
		final FacesContext ctx = FacesContext.getCurrentInstance();
		final String msg = bundle.getString(msgKey);

		ctx.addMessage(null, new FacesMessage(severity, msg, null));
	}

	/**
	 * @param bundle
	 * @param severity
	 * @param msgKey
	 * @param t
	 */
	public static void sendFacesMessage(ResourceBundle bundle, FacesMessage.Severity severity, String msgKey, Throwable t) {
		sendFacesMessage(bundle, severity, msgKey, ExceptionHelper.getRootCause(t).getMessage());
	}

	/**
	 * @param bundle
	 * @param severity
	 * @param msgKey
	 * @param details
	 * @param args
	 */
	public static void sendFacesMessage(ResourceBundle bundle, FacesMessage.Severity severity, String msgKey, String details,
			Object... args) {
		final FacesContext ctx = FacesContext.getCurrentInstance();
		String msg = bundle.getString(msgKey);

		if (args.length > 0) {
			final var format = new MessageFormat(msg);
			msg = format.format(args);
		}

		ctx.addMessage(null, new FacesMessage(severity, msg, details));
	}

}
